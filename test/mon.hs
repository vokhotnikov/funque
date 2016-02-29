{-# LANGUAGE ViewPatterns #-}

import System.Environment(getArgs)
import System.Exit(exitFailure)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import Data.ByteString.Internal(c2w,w2c)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Control.Concurrent.Chan
import Data.Word(Word8)
import Text.Parsec((<?>), many, many1, optionMaybe, parse)
import Text.Parsec.ByteString
import Text.Parsec.Prim(tokenPrim)
import Text.Parsec.Pos (updatePosChar, updatePosString)

type StompVersion = String

type StompHeader = (String, String)

type StompBody = [Word8]

data StompCommand = StompConnect 
                  | StompDisconnect

data StompEvent = StompConnected StompVersion

data StompClient = StompClient { scSocket :: Socket, scReceiveChan :: Chan String }

data StompFrame = StompFrame String [StompHeader] (Maybe StompBody) deriving (Show)

instance Show StompEvent where
    show (StompConnected version) = "Connected (ver. " ++ version ++ ")"

satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy f   = tokenPrim (\c -> show [c])
                        (\pos c _cs -> updatePosChar pos c)
                        (\c -> if f (c2w c) then Just (c2w c) else Nothing)

p_lf :: Parser Word8
p_lf = satisfy (==10) <?> "newline"

p_null :: Parser Word8
p_null = satisfy(==0) <?> "NULL byte"

p_anyCharExcept :: [Word8] -> Parser Char
p_anyCharExcept e = w2c `fmap` satisfy(\b -> b >= 0 && b <= 127 && (b `notElem` e))

p_anyChar = p_anyCharExcept []

p_char :: Char -> Parser Char
p_char c = do
    r <- satisfy $ (==) $ c2w c
    return (w2c r)

p_string :: String -> Parser String
p_string [] = satisfy (const True) >> return ""
p_string (x:xs) = do
    c <- p_char x
    cs <- p_string xs
    return (c:cs)

p_octet :: Parser Word8
p_octet = satisfy (const True) <?> "octet"

p_notNull :: Parser Word8
p_notNull = satisfy (/=0) <?> "not-NULL byte"

p_frame :: Parser StompFrame
p_frame = do
    cmd <- p_command
    p_lf
    headers <- many p_header
    p_lf
    body <- optionMaybe $ many p_notNull
    p_null
    many p_lf
    return $ StompFrame cmd headers body
    

p_command :: Parser String
p_command = many1 $ p_anyCharExcept [10]

p_header :: Parser StompHeader
p_header = do
    name <- many1 $ p_anyCharExcept [10, (c2w ':')]
    p_char ':'
    value <- many1 $ p_anyCharExcept [10]
    p_lf
    return (name, value)

stompSend :: StompClient -> StompCommand -> IO()
stompSend client cmd = do
    send (scSocket client) $ B8.pack . bytes $ cmd 
    return ()
  where 
    bytes StompConnect = "CONNECT\n\n\NUL"
    bytes StompDisconnect = "DISCONNECT\n\n\NUL"

stompRecv :: StompClient -> IO B.ByteString
stompRecv client = 
    recv (scSocket client) 4096

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
        received <- (parse p_frame "received frame") `fmap` stompRecv client
        either print print received
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
