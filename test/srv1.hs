import Funque.Stomp.Parser
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import Control.Exception (bracket, finally)
import qualified Data.ByteString as B
import Data.Word (Word16)

type Host = String

data MyIncomingMessage = ListVpnExclusions
                       | ExcludeFromVpn Host
                       | RemoveVpnExclusionFor Host

data MyOutgoingMessage = CurrentVpnExclusions [Host]
                       | HostExcludedFromVpn Host
                       | HostVpnExclusionRemoved Host

type BrokerConfig = String

type BrokerContext = ()

type Handler a = BrokerContext -> a -> IO ()

type StompVersion = String

type StompDestination = String

data StompAckMode = ClientAck | AutoAck

type StompMessageId = String

type StompReceiptId = String

type StompMessageBody = String

data StompCommand = StompConnect
                  | StompDisconnect
                  | SubscribeTo StompDestination StompAckMode
                  | UnsubscribeFrom StompDestination
                  | AckMessage StompMessageId

data StompEvent = StompConnected StompVersion
                | StompMessage StompDestination StompMessageId StompMessageBody [StompHeader]
                | StompReceipt StompReceiptId
                | StompError { getStompErrorMessage :: String, getStompErrorDetails :: Maybe String }

instance Show StompEvent where
    show (StompConnected version) = "Connected (ver. " ++ version ++ ")"
    show _ = undefined

stompSend :: Socket -> StompCommand -> IO ()
stompSend sock cmd = do 
    send sock $ (serializeFrame . toFrame) cmd
    return ()
  where
    toFrame StompConnect = StompFrame "CONNECT" [] Nothing
    toFrame StompDisconnect = StompFrame "DISCONNECT" [] Nothing
    toFrame _ = undefined
    

stompReceive :: Socket -> IO (Either StompParseError StompEvent)
stompReceive = undefined

withBroker :: BrokerConfig -> (BrokerContext -> IO a) -> IO a
withBroker host act = withSocketsDo $ bracket (connectTo host 61613) close $ \sock -> do
    stompSend sock StompConnect
    received <- stompReceive sock
    either print print received
    (act ()) `finally` (stompSend sock StompDisconnect)
  where 
    connectTo :: String -> Word16 -> IO Socket
    connectTo host port = do
      addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
      let serverAddr = head addrInfo
      sock <- socket (addrFamily serverAddr) Stream defaultProtocol
      setSocketOption sock ReuseAddr 1 
      putStrLn $ "About to connect to " ++ (show . addrAddress $ serverAddr)
      connect sock (addrAddress serverAddr)
      return sock

main = do
    brokerConfig <- resolveConfig
    withBroker brokerConfig $ \ctx -> 
      withHandler ctx handleIncoming $ \_ -> do
        putStrLn "Press a key to stop.."
        getChar

  where resolveConfig = return "localhost"
        handleIncoming :: Handler MyIncomingMessage
        handleIncoming ctx ListVpnExclusions = replyWith ctx $ CurrentVpnExclusions ["192.168.222.104"]  
        handleIncoming _ _ = undefined
        withHandler :: BrokerContext -> Handler a -> (BrokerContext -> IO b) -> IO b
        withHandler = undefined
        replyWith :: BrokerContext -> a -> IO BrokerContext
        replyWith = undefined
        

