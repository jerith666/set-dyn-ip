-- nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [network amazonka amazonka-route53])"

import System.Environment
import System.IO

import Data.List.NonEmpty

import Network.Socket hiding (send, sendTo, recv, recvFrom)

import Control.Monad.Trans.AWS

import Network.AWS.Auth
import Network.AWS.Data.Text
import Network.AWS.Env
import Network.AWS.Route53.Types
import Network.AWS.Route53.ChangeResourceRecordSets

main = do args <- getArgs
          case args of
               []        -> usage
               (p:[])    -> usage
               (h:p:[])  -> setDynIpStr h p
               (x:y:z:_) -> usage

setDynIpStr :: String -> String -> IO ()
setDynIpStr host portStr = do case (reads portStr) of
                                   [] -> usage
                                   [(p, _)] -> setDynIp host p

setDynIp :: String -> Integer -> IO ()
setDynIp host port = do ios <- socket AF_INET Stream defaultProtocol
                        hostInfos <- getAddrInfo (Just defaultHints) (Just host) Nothing
                        setDynIpH hostInfos port ios

setDynIpH :: [AddrInfo] -> Integer -> Socket -> IO ()
setDynIpH hostInfos port ios =
  case hostInfos of
    [] -> usage
    (hostInfo:_) ->
      case (addrAddress hostInfo) of
        SockAddrInet _ addr ->
          do connect ios (SockAddrInet (fromInteger port) addr)
             ioh <- socketToHandle ios ReadMode
             externIp <- hGetLine ioh
             hClose ioh
             changeIpAddr externIp
        _ -> usage

changeIpAddr :: String -> IO ()
changeIpAddr externIp = do
    putStrLn $ "got external ip " ++ externIp ++ "."
    env <- newEnv NorthVirginia
                  (FromEnv (toText "AWS_ACCESS_KEY")
                           (toText "AWS_SECRET_KEY")
                           Nothing)
    runResourceT . runAWST env $ do
        send $ changeResourceRecordSets (toText "hostedZoneId")
                                        (changeBatch $ change Upsert
                                                              (resourceRecordSet (toText "name")
                                                                                 A)
                                                       :| [] )
    return ()

usage = putStrLn "usage: set-dyn-ip host port"
