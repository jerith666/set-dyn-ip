-- nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [network amazonka amazonka-route53])"

import Control.Concurrent (threadDelay)
import Control.Lens ((&), (?~))
import Control.Monad.Trans.AWS
  ( runAWST,
    runResourceT,
    send,
  )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Time.Clock (getCurrentTime)
import Network.AWS.Auth (Credentials (FromEnv))
import Network.AWS.Data.Text (ToText (toText))
import Network.AWS.Env (newEnv)
import Network.AWS.Route53.ChangeResourceRecordSets
  ( changeResourceRecordSets,
  )
import Network.AWS.Route53.Types
  ( ChangeAction (Upsert),
    RecordType (A),
    ResourceId (ResourceId),
    change,
    changeBatch,
    resourceRecord,
    resourceRecordSet,
    rrsResourceRecords,
    rrsTTL,
  )
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
    [] -> usage
    [(p, _)] -> setDynIp zone tgtHosts host p

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
  currentIp <- getCurrentIpAddr host
  if currentIp == externIp then
    putStrLn $ "The current IP address is already " ++ externIp
  else do
    env <-
      newEnv
        ( FromEnv
            (toText "AWS_ACCESS_KEY")
            (toText "AWS_SECRET_KEY")
            Nothing
            Nothing
        )
    runResourceT . runAWST env $ do
      let rrs =
            resourceRecordSet (toText host) A
              & rrsResourceRecords ?~ resourceRecord (toText externIp) :| []
              & rrsTTL ?~ 300
      send $
        changeResourceRecordSets
          (ResourceId (toText zone))
          ( changeBatch $
              change
                Upsert
                rrs
                :| []
          )
    t <- getCurrentTime
    putStrLn $ "at " ++ show t ++ " set A record for " ++ host ++ " = " ++ externIp
    threadDelay perReqDelay
  return ()

-- route53 throttles at 5 req/sec
-- threadDelay takes units of microseconds
-- go slower than that to be sure
perReqDelay :: Int
perReqDelay = 1000000

getCurrentIpAddr :: String -> IO String
getCurrentIpAddr host = do
  -- Fetch the current IP address from Amazon AWS Route53
  -- Handle any errors that might occur during the fetch operation
  -- Return the current IP address as a string

usage :: IO ()
usage = putStrLn "usage: set-dyn-ip zoneid host port tgthost1 [tgthost2 ...]"
