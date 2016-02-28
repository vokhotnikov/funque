{-# LANGUAGE ViewPatterns #-}

import System.Environment(getArgs)
import System.Exit(exitFailure)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Control.Concurrent.Chan
import Data.Word(Word8)
import Text.Parsec.ByteString

type StompVersion = String

type StompHeader = (String, String)

data StompCommand = StompConnect 
                  | StompDisconnect

data StompEvent = StompConnected StompVersion
                | StompGenericEvent String [StompHeader] (Maybe [Word8])

data StompClient = StompClient { scSocket :: Socket, scReceiveChan :: Chan String }


stompSend :: StompClient -> StompCommand -> IO()
stompSend client cmd = do
    send (scSocket client) $ B8.pack . bytes $ cmd 
    return ()
  where 
    bytes StompConnect = "CONNECT\n\n\NUL"
    bytes StompDisconnect = "DISCONNECT\n\n\NUL"

stompRecv :: StompClient -> IO String
stompRecv client = 
    B8.unpack `fmap` recv (scSocket client) 4096

main = do
    args <- getArgs
    case args of
      [host, reads -> [(port,_)]] -> withClientDo host port mainLoop
      [host] -> withClientDo host 61613 mainLoop 
      _ -> do
        putStrLn "UsageL mon <host> [port]"
        exitFailure
    where
      mainLoop :: StompClient -> IO ()
      mainLoop client = do
        stompSend client StompConnect
        putStrLn =<< stompRecv client
        putStrLn "Press a key to interrupt..."
        getChar
        putStrLn "...interrupted"
        stompSend client StompDisconnect

withClientDo :: String -> Int -> (StompClient -> IO b) -> IO b
withClientDo host port act = do
    sock <- connectTo host port
    putStrLn "Connected"
    chan <- newChan
    let client = StompClient sock chan
    res <- act client
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
