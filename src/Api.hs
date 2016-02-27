module Funque.Api

data BrokerConfig = Broker { getHost:: String, getPort :: UInt16 } 

resolveConfig :: () -> IO BrokerConfig
resolveConfig = Broker "localhost" 61613

