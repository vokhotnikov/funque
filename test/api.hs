import Data.Word(Word16)
import Control.Concurrent(threadDelay)

type EntityType = String
type EntityId = Int

class IncomingMessage a where
    inDestinationName :: a -> String

class OutgoingMessage a where
    outDestinationName :: a -> String

data IndexEntities = IndexEntities EntityType [EntityId]

data SearchQuery = SearchQuery EntityType String 

data SearchQueryResult = SearchQueryResult EntityType String EntityId

data EntitiesIndexed = EntitiesIndexed EntityType EntityId

type Context = String

data BrokerConfig = BrokerConfig { brokerHost :: String, brokerPort :: Word16 }

resolveConfig :: () => IO BrokerConfig
resolveConfig = return $ BrokerConfig "localhost" 61613

withBroker:: BrokerConfig -> (Context -> IO a) -> IO a
withBroker = undefined

send :: OutgoingMessage a => Context -> a -> IO ()
send = undefined

withHandler :: IncomingMessage a => (a -> Context -> IO b) -> Context -> (Context -> IO c) -> IO c
withHandler = undefined

---main = do
---    brokerConfig <- resolveConfig
---    withBroker brokerConfig $ \ctx ->
---      withHandler indexedHandler ctx $ \() -> 
---        send ctx $ IndexEntities "Flash" [2001..2005]
---    where
---      indexedHandler msg ctx = do 
---          putStrLn "Indexed entities, issuing query..."
---          withHandler searchHandler ctx $ \() -> 
---            send ctx $ SearchQuery "Flash" "DTT"
---      searchHandler (SearchQueryResult _ _ ids) ctx =
---        putStrLn "Got " ++ (length ids) ++ " results."

handleIndexed :: EntitiesIndexed -> Context -> IO ()
handleIndexed = undefined

handleSearchResult :: SearchQueryResult -> Context -> IO ()
handleSearchResult = undefined

main1 = do
    brokerConfig <- resolveConfig
    withBroker brokerConfig $ \ctx ->
      withHandler (handleIndexed) ctx $ \ctx ->
      withHandler (handleSearchResult) ctx $ \ctx ->
        threadDelay 1000000 -- do some "work"
        

      
instance IncomingMessage EntitiesIndexed where
    inDestinationName _ = "topic1"
