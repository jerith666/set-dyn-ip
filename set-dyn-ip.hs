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
               []            -> usage
               (z:[])        -> usage
               (z:t:[])      -> usage
               (z:t:h:[])    -> usage
               (z:t:h:p:[])  -> setDynIpStr z t h p
               (x:y:z:w:v:_) -> usage

setDynIpStr :: String -> String -> String -> String -> IO ()
setDynIpStr zone tgtHost host portStr = do case (reads portStr) of
                                                [] -> usage
                                                [(p, _)] -> setDynIp zone tgtHost host p

setDynIp :: String -> String -> String -> Integer -> IO ()
setDynIp zone tgtHost host port = do ios <- socket AF_INET Stream defaultProtocol
                                     hostInfos <- getAddrInfo (Just defaultHints) (Just host) Nothing
                                     setDynIpH zone tgtHost hostInfos port ios

setDynIpH :: String -> String -> [AddrInfo] -> Integer -> Socket -> IO ()
setDynIpH zone host hostInfos port ios =
  case hostInfos of
    [] -> usage
    (hostInfo:_) ->
      case (addrAddress hostInfo) of
        SockAddrInet _ addr ->
          do connect ios (SockAddrInet (fromInteger port) addr)
             ioh <- socketToHandle ios ReadMode
             externIp <- hGetLine ioh
             hClose ioh
             changeIpAddr zone host externIp
        _ -> usage

changeIpAddr :: String -> String -> String -> IO ()
changeIpAddr zone host externIp = do
    putStrLn $ "got external ip " ++ externIp ++ "."
    logg <- newLogger Trace stdout
    env <- newEnv NorthVirginia
                  (FromEnv (toText "AWS_ACCESS_KEY")
                           (toText "AWS_SECRET_KEY")
                           Nothing)
    runResourceT . runAWST (env & envLogger .~ logg) $ do
        let rrs = resourceRecordSet (toText host) A
                & rrsResourceRecords ?~ resourceRecord (toText externIp) :| []
        send $ changeResourceRecordSets (toText zone)
                                        (changeBatch $ change Upsert
                                                              rrs
                                                       :| [] )
    return ()

usage = putStrLn "usage: set-dyn-ip zoneid tgthost host port"
