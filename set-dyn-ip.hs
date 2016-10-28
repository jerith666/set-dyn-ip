-- nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [network amazonka amazonka-route53])"

import System.Environment
import System.IO

import Data.List.NonEmpty

import Network.Socket hiding (send, sendTo, recv, recvFrom)

import Control.Lens
import Control.Monad.Trans.AWS

import Network.AWS.Auth
import Network.AWS.Data.Text
import Network.AWS.Env
import Network.AWS.Route53.Types
import Network.AWS.Route53.ChangeResourceRecordSets

main = do args <- getArgs
          case args of
               []          -> usage
               (z:[])      -> usage
               (z:h:[])    -> usage
               (z:h:p:[])  -> setDynIpStr z h p
               (x:y:z:w:_) -> usage

setDynIpStr :: String -> String -> String -> IO ()
setDynIpStr zone host portStr = do case (reads portStr) of
                                        [] -> usage
                                        [(p, _)] -> setDynIp zone host p

setDynIp :: String -> String -> Integer -> IO ()
setDynIp zone host port = do ios <- socket AF_INET Stream defaultProtocol
                             hostInfos <- getAddrInfo (Just defaultHints) (Just host) Nothing
                             setDynIpH zone hostInfos port ios

setDynIpH :: String -> [AddrInfo] -> Integer -> Socket -> IO ()
setDynIpH zone hostInfos port ios =
  case hostInfos of
    [] -> usage
    (hostInfo:_) ->
      case (addrAddress hostInfo) of
        SockAddrInet _ addr ->
          do connect ios (SockAddrInet (fromInteger port) addr)
             ioh <- socketToHandle ios ReadMode
             externIp <- hGetLine ioh
             hClose ioh
             changeIpAddr zone externIp
        _ -> usage

changeIpAddr :: String -> String -> IO ()
changeIpAddr zone externIp = do
    putStrLn $ "got external ip " ++ externIp ++ "."
    env <- newEnv NorthVirginia
                  (FromEnv (toText "AWS_ACCESS_KEY")
                           (toText "AWS_SECRET_KEY")
                           Nothing)
    runResourceT . runAWST env $ do
        rrs <- resourceRecordSet (toText "name") A
        rrs2 <- set rrsResourceRecords
                    (Just (resourceRecord (toText externIp) :| []))
                    rrs
        send $ changeResourceRecordSets (toText zone)
                                        (changeBatch $ change Upsert
                                                              rrs2
                                                       :| [] )
    return ()

usage = putStrLn "usage: set-dyn-ip zoneid host port"
