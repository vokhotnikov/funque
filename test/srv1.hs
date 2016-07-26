import Funque.Stomp.Parser
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import Control.Exception (bracket, finally)
import qualified Data.ByteString as B
import Data.Word (Word16)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (forever)

type Host = String

data MyIncomingMessage = ListVpnExclusions
                       | ExcludeFromVpn Host
                       | RemoveVpnExclusionFor Host

data MyOutgoingMessage = CurrentVpnExclusions [Host]
                       | HostExcludedFromVpn Host
                       | HostVpnExclusionRemoved Host

type BrokerConfig = String

newtype BrokerContext = BrokerContext { bcSocket :: Socket }

type Handler a = BrokerContext -> a -> IO ()

type StompVersion = String

type StompDestination = String

data StompAckMode = StompClientAck | StompAutoAck

type StompMessageId = String

type StompReceiptId = String

type StompMessageBody = String

data StompCommand = StompConnect
                  | StompDisconnect
                  | StompSubscribeTo StompDestination StompAckMode
                  | StompUnsubscribeFrom StompDestination
                  | StompAckMessage StompMessageId

data StompEvent = StompConnected StompVersion
                | StompMessage StompDestination StompMessageId StompMessageBody [StompHeader]
                | StompReceipt StompReceiptId
                | StompError { getStompErrorMessage :: String, getStompErrorDetails :: Maybe String }
                | StompGenericEvent StompFrame

instance Show StompEvent where
    show (StompConnected version) = "Connected (ver. " ++ version ++ ")"
    show _ = undefined

stompSend :: Socket -> StompCommand -> IO ()
stompSend sock cmd = do 
    let frame = toFrame cmd
    send sock $ serializeFrame frame
    putStrLn $ "Sent: " ++ (show frame)
    return ()
  where
    toFrame :: StompCommand -> StompFrame
    toFrame StompConnect = StompFrame "CONNECT" [] Nothing
    toFrame StompDisconnect = StompFrame "DISCONNECT" [] Nothing
    toFrame (StompSubscribeTo d ack) = 
        StompFrame "SUBSCRIBE" [("destination", d), ("ack", ackValue ack)] Nothing
    toFrame (StompUnsubscribeFrom d) = 
        StompFrame "UNSUBSCRIBE" [("destination", d)] Nothing
    toFrame _ = undefined
    ackValue :: StompAckMode -> String
    ackValue StompClientAck = "client"
    ackValue StompAutoAck= "auto"


stompReceive :: Socket -> IO (Either StompParseError StompEvent)
stompReceive sock = do
    bytes <- recv sock 4096
    -- TODO: handle zero length result
    -- TODO: what to do for longer messages?
    let frame = parseFrame bytes
    putStrLn $ "Got: " ++ (either show show frame)
    return $ frameToEvent `fmap` frame
  where
    frameToEvent :: StompFrame -> StompEvent
    frameToEvent (StompFrame "CONNECTED" hs Nothing) = StompConnected $ fromMaybe "1.0" (lookup "version" hs)
    frameToEvent f = StompGenericEvent f

withBroker :: BrokerConfig -> (BrokerContext -> IO a) -> IO a
withBroker host act = withSocketsDo $ bracket (connectTo host 61613) close $ \sock -> do
    ch <- newChan
    bracket (forkIO $ receiveLoop sock ch) killThread $ \_ -> do
      stompSend sock StompConnect
      received <- readChan ch
      either print print received
      (act $ BrokerContext sock) `finally` (stompSend sock StompDisconnect)
  where 
    connectTo :: String -> Word16 -> IO Socket
    connectTo host port = do
      addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
      let serverAddr = head addrInfo
      sock <- socket (addrFamily serverAddr) Stream defaultProtocol
      setSocketOption sock ReuseAddr 1 
      setSocketOption sock KeepAlive 1 
      putStrLn $ "About to connect to " ++ (show . addrAddress $ serverAddr)
      connect sock (addrAddress serverAddr)
      return sock
    receiveLoop :: Socket -> Chan (Either StompParseError StompEvent) -> IO ()
    receiveLoop sock ch = forever $ do
      m <- stompReceive sock
      m `seq` writeChan ch m

main = do
    brokerConfig <- resolveConfig
    withBroker brokerConfig $ \ctx ->
      withHandler ctx handleIncoming $ \_ -> do
        putStrLn "Press a key to stop.."
        getChar

  where resolveConfig = return "127.0.0.1"
        handleIncoming :: Handler MyIncomingMessage
        handleIncoming ctx ListVpnExclusions = do 
            --replyWith ctx $ CurrentVpnExclusions ["192.168.222.104"]  
            return ()
        handleIncoming _ _ =  return ()
        withHandler :: BrokerContext -> Handler a -> (BrokerContext -> IO b) -> IO b
        withHandler ctx _ act = let
            sock = bcSocket ctx
            cmdSub = StompSubscribeTo "test.topic" StompAutoAck
            cmdUnsub = StompUnsubscribeFrom "test.topic"
          in do
            bracket (stompSend sock cmdSub) (const $ stompSend sock cmdUnsub) (const $ act ctx)
        replyWith :: BrokerContext -> a -> IO BrokerContext
        replyWith = undefined
        

