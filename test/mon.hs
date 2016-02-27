{-# LANGUAGE ViewPatterns #-}

import System.Environment(getArgs)
import System.Exit(exitFailure)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString as B
import Control.Concurrent.Chan

type Destination = String
type Message = String
type SubscribeCallback = (Destination -> IO ())

data StompState = Disconnected
                | SocketConnected
                | ClientReady
                | ClientSending Destination Message 
                | SubscriptionPending Destination
                | UnsubscriptionPending Destination
                | ClientHandling Destination Message
                | Disconnecting

main = do
    args <- getArgs
    case args of
      [host, reads -> [(port,_)]] -> withClientDo host port mainLoop
      [host] -> withClientDo host 61613 mainLoop 
      _ -> do
        putStrLn "UsageL mon <host> [port]"
        exitFailure
    where
      mainLoop :: Chan a -> IO ()
      mainLoop _ = do
        putStrLn "Press a key to interrupt..."
        getChar
        putStrLn "...interrupted"

withClientDo :: String -> Int -> (Chan a -> IO b) -> IO b
withClientDo host port act = do
    sock <- connectTo host port
    putStrLn "Connected"
    chan <- newChan
    res <- act chan
    sClose sock
    putStrLn "Disconnected"
    return res
  where 
    connectTo host port = do
      addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
      let serverAddr = head addrInfo
      sock <- socket (addrFamily serverAddr) Stream defaultProtocol
      setSocketOption sock ReuseAddr 1 
      putStrLn $ "About to connect to " ++ (show . addrAddress $ serverAddr)
      connect sock (addrAddress serverAddr)
      return sock
