
import System.Environment
import System.IO
import Network.Socket hiding (send, sendTo, recv, recvFrom)

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
             putStrLn $ "got external ip " ++ externIp ++ "."
        _ -> usage

usage = putStrLn "usage: set-dyn-ip host port"
