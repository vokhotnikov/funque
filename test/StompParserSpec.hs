module StompParserSpec where

import Funque.Stomp.Parser
import Data.ByteString
import Data.ByteString.Internal(c2w)

import Test.HUnit

testParseConnected = TestCase (assertEqual "parsing connected" 
                              (Right (StompFrame "CONNECTED" [
                                                     ("session", "ID:123"),
                                                     ("heartbeat", "0,0"),
                                                     ("version", "1.0")]
                                                     Nothing))
                                                     (parseFrame . pack . (fmap c2w) $ "CONNECTED\nsession:ID:123\nheartbeat:0,0\nversion:1.0\n\n\NUL"))

