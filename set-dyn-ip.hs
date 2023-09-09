-- nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [network amazonka amazonka-route53])"

import System.Environment
import System.IO

import Data.Time.Clock

import Data.List.NonEmpty

import Network.Socket hiding (send, sendTo, recv, recvFrom)

import Control.Concurrent
import Control.Lens
import Control.Monad.Trans.AWS

import Network.AWS.Auth
import Network.AWS.Data.Text
import Network.AWS.Env
import Network.AWS.Route53.Types
import Network.AWS.Route53.ChangeResourceRecordSets

main = do args <- getArgs
          case args of
               []            -> usage
               (z:[])        -> usage
               (z:h:[])      -> usage
               (z:h:p:[])    -> usage
               (z:h:p:t:[])  -> setDynIpStr z [t] h p
               (z:h:p:t:ts)  -> setDynIpStr z (t:ts) h p

setDynIpStr :: String -> [String] -> String -> String -> IO ()
setDynIpStr zone tgtHosts host portStr = do case (reads portStr) of
                                                 [] -> usage
                                                 [(p, _)] -> setDynIp zone tgtHosts host p

setDynIp :: String -> [String] -> String -> Integer -> IO ()
setDynIp zone tgtHosts host port = do ios <- socket AF_INET Stream defaultProtocol
                                      hostInfos <- getAddrInfo (Just defaultHints) (Just host) Nothing
                                      setDynIpH zone tgtHosts hostInfos port ios

setDynIpH :: String -> [String] -> [AddrInfo] -> Integer -> Socket -> IO ()
setDynIpH zone hosts hostInfos port ios =
  case hostInfos of
    [] -> usage
    (hostInfo:_) ->
      case (addrAddress hostInfo) of
        SockAddrInet _ addr ->
          do connect ios (SockAddrInet (fromInteger port) addr)
             ioh <- socketToHandle ios ReadMode
             externIp <- hGetLine ioh
             hClose ioh
             changeIpAddrs zone hosts externIp
        _ -> usage

changeIpAddrs :: String -> [String] -> String -> IO ()
changeIpAddrs zone hosts externIp = do
    mapM (\h -> changeIpAddr zone h externIp) hosts
    return ()

changeIpAddr :: String -> String -> String -> IO ()
changeIpAddr zone host externIp = do
    currentIp <- getCurrentIpAddr zone host
    if currentIp == Just externIp
        then putStrLn $ "The current IP address for " ++ host ++ " already matches the desired value. No changes were made."
        else do
            env <- createEnv
            runResourceT . runAWST env $ do
                let rrs = resourceRecordSet (toText host) A
                        & rrsResourceRecords ?~ resourceRecord (toText externIp) :| []
                        & rrsTTL ?~ 300
                send $ changeResourceRecordSets (ResourceId (toText zone))
                                                (changeBatch $ change Upsert
                                                                  rrs
                                                           :| [] )
            t <- getCurrentTime
            putStrLn $ "at " ++ (show t ) ++ " set A record for " ++ host ++ " = " ++ externIp
            threadDelay perReqDelay
    return ()

-- route53 throttles at 5 req/sec
-- threadDelay takes units of microseconds
-- go slower than that to be sure
perReqDelay :: Int
perReqDelay = 1000000

createEnv :: IO Env
createEnv = newEnv (FromEnv (toText "AWS_ACCESS_KEY")
                            (toText "AWS_SECRET_KEY")
                            Nothing
                            Nothing)

getCurrentIpAddr :: String -> String -> IO (Maybe String)
getCurrentIpAddr zone host = do
    env <- createEnv
    runResourceT . runAWST env $ do
        resp <- send $ listResourceRecordSets (ResourceId (toText zone))
        return $ find ((== toText host) . view rrsName) (view lrrsResourceRecordSets resp)
                  >>= listToMaybe . map (view rrValue) . fromMaybe [] . view rrsResourceRecords

usage = putStrLn "usage: set-dyn-ip zoneid host port tgthost1 [tgthost2 ...]"
