{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

{-# HLINT ignore "Use if" #-}

import Amazonka (Env, discover, runResourceT, send)
import Amazonka.Data.Text (ToText (toText), fromText)
import Amazonka.Env (newEnv)
import Amazonka.Route53
  ( ChangeAction (ChangeAction_UPSERT),
    RRType (RRType_A),
    newChange,
    newChangeBatch,
    newChangeResourceRecordSets,
    newListResourceRecordSets,
    newResourceRecord,
    newResourceRecordSet,
  )
import Amazonka.Route53.Lens
  ( changeResourceRecordSetsResponse_httpStatus,
    listResourceRecordSetsResponse_httpStatus,
    listResourceRecordSetsResponse_resourceRecordSets,
    listResourceRecordSets_startRecordName,
    listResourceRecordSets_startRecordType,
    resourceRecordSet_name,
    resourceRecordSet_type,
    resourceRecord_value,
  )
import Amazonka.Route53.Types
  ( ResourceId (ResourceId),
    resourceRecordSet_resourceRecords,
    resourceRecordSet_ttl,
  )
import Control.Concurrent (threadDelay)
import Control.Lens ((&), (?~))
import Control.Lens.Getter (view)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Time.Clock (getCurrentTime)
import Network.Socket
  ( AddrInfo (addrAddress),
    Family (AF_INET),
    SockAddr (SockAddrInet),
    Socket,
    SocketType (Stream),
    connect,
    defaultHints,
    defaultProtocol,
    getAddrInfo,
    socket,
    socketToHandle,
  )
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hClose, hGetLine)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [z, h, p, t] -> setDynIpStr z [t] h p
    (z : h : p : t : ts) -> setDynIpStr z (t : ts) h p
    _ -> usage

setDynIpStr :: String -> [String] -> String -> String -> IO ()
setDynIpStr zone tgtHosts host portStr = do
  case reads portStr of
    [(p, _)] -> setDynIp zone tgtHosts host p
    _ -> usage

setDynIp :: String -> [String] -> String -> Integer -> IO ()
setDynIp zone tgtHosts host port = do
  ios <- socket AF_INET Stream defaultProtocol
  hostInfos <- getAddrInfo (Just defaultHints) (Just host) Nothing
  setDynIpH zone tgtHosts hostInfos port ios

setDynIpH :: String -> [String] -> [AddrInfo] -> Integer -> Socket -> IO ()
setDynIpH zone hosts hostInfos port ios =
  case hostInfos of
    [] -> usage
    (hostInfo : _) ->
      case addrAddress hostInfo of
        SockAddrInet _ addr ->
          do
            connect ios (SockAddrInet (fromInteger port) addr)
            ioh <- socketToHandle ios ReadMode
            externIp <- hGetLine ioh
            hClose ioh
            changeIpAddrs zone hosts externIp
        _ -> usage

changeIpAddrs :: String -> [String] -> String -> IO ()
changeIpAddrs zone hosts externIp = do
  mapM_ (\h -> changeIpAddr zone h externIp) hosts

changeIpAddr :: String -> String -> String -> IO ()
changeIpAddr zone host externIp = do
  env <- newEnv discover
  currentIp <- getIpAddr env zone host
  case currentIp == Just externIp of
    True -> do
      t <- getCurrentTime
      putStrLn $ "at " ++ show t ++ " A record for " ++ host ++ " already is " ++ externIp
      return ()
    False -> do
      let rrs =
            newResourceRecordSet (toText host) RRType_A
              & resourceRecordSet_resourceRecords ?~ newResourceRecord (toText externIp) :| []
              & resourceRecordSet_ttl ?~ 300
          changeRrs =
            newChangeResourceRecordSets
              (ResourceId (toText zone))
              ( newChangeBatch $
                  newChange
                    ChangeAction_UPSERT
                    rrs
                    :| []
              )
      changeRrsResp <- runResourceT $ send env changeRrs
      t <- getCurrentTime
      let respCode = view changeResourceRecordSetsResponse_httpStatus changeRrsResp
          respDescr = case respCode < 400 of
            True -> " successfully set A record for "
            False -> " failed (" ++ show respCode ++ ") to set A record for "
      putStrLn $ "at " ++ show t ++ respDescr ++ host ++ " = " ++ externIp
      threadDelay perReqDelay
      return ()

getIpAddr :: Env -> String -> String -> IO (Maybe String)
getIpAddr env zone host = do
  let listRrs =
        newListResourceRecordSets (ResourceId (toText zone))
          & listResourceRecordSets_startRecordName ?~ toText host
          & listResourceRecordSets_startRecordType ?~ RRType_A
  listRrsResp <- runResourceT $ send env listRrs
  let respCode = view listResourceRecordSetsResponse_httpStatus listRrsResp
  case respCode < 400 of
    False -> return Nothing
    True ->
      case view listResourceRecordSetsResponse_resourceRecordSets listRrsResp of
        rrSet : _ ->
          let name = view resourceRecordSet_name rrSet
              typE = view resourceRecordSet_type rrSet
              mRRs = view resourceRecordSet_resourceRecords rrSet
           in case (name == toText host || (name == toText (host ++ "."))) && typE == RRType_A of
                False -> return Nothing
                True ->
                  case mRRs of
                    Nothing -> return Nothing
                    Just rrs ->
                      case rrs of
                        rr :| [] ->
                          case fromText $ view resourceRecord_value rr of
                            Right rrValue -> return $ Just rrValue
                            Left _ -> return Nothing
                        _ -> return Nothing
        _ -> return Nothing

-- route53 throttles at 5 req/sec
-- threadDelay takes units of microseconds
-- go slower than that to be sure
perReqDelay :: Int
perReqDelay = 1000000

usage :: IO ()
usage = putStrLn "usage: set-dyn-ip zoneid host port tgthost1 [tgthost2 ...]"
