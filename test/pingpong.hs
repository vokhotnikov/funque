import System.Environment
import Funque.Stomp.Parser

newtype BrokerContext = BrokerContext ()

type BrokerFactory = IO BrokerContext

type ApiHandlers a = a -> BrokerContext -> IO BrokerContext

makeBroker :: ApiHandlers a -> BrokerFactory
makeBroker _ = return $ BrokerContext ()

withBroker :: BrokerFactory -> (BrokerContext -> IO a) -> IO a
withBroker bf act = do
    ctx <- bf
    act ctx

sendMessage :: BrokerContext -> a -> IO BrokerContext
sendMessage ctx m = undefined

data PingerApi = Ping Int

data PongerApi = Pong Int

handlePing id ctx = do
    putStrLn $ "Got ping " ++ (show id)
    sendMessage ctx $ Pong id

handlePong id = putStrLn $ "Got pong " ++ (show id)

runTillKeyPressed :: IO ()
runTillKeyPressed = do
    putStrLn "Press a key to stop.."
    getChar
    return ()

runPinger :: IO ()
runPinger = withBroker (makeBroker pingerHandlers) $ \ctx -> do
    mapM_ (\id -> sendMessage ctx $ Ping id) [1..10]
    runTillKeyPressed
  where pingerHandlers (Ping id) = handlePing id

runPonger :: IO ()
runPonger = withBroker (makeBroker pongerHandlers) $ \_ -> runTillKeyPressed
  where pongerHandlers (Pong id) ctx = handlePong id >> return ctx

main = do
  args <- getArgs
  case args of
    ["ping"] -> runPinger
    ["pong"] -> runPonger
    _ -> usage 
  where usage = putStrLn "Usage: pingpong (ping|pong)"

  

