module StompParserSpec where

import Funque.Stomp.Parser
import Data.ByteString(pack)
import Data.ByteString.Internal(c2w, w2c)
import Data.Word(Word8)
import Control.Monad

import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

tests = [ testWhitespaceHeaders
        , testParseConnected
        , testParseNoHeaders
        , testParseBody
        , testQCase1
        , testProperty "serialize and parse" prop_serializeAndParse
        ]

parseCheck name s frame = testCase name $ assertEqual name (Right frame) (parseFrame . pack . (fmap c2w) $ s)

serializeAndParseCheck name f = testCase name $ assertEqual name (Right f) $ (parseFrame . serializeFrame) f

testWhitespaceHeaders = parseCheck "whitespace in header value" "CONNECTED\nversion: 1.0\n\n\NUL" $
    StompFrame "CONNECTED" [("version", " 1.0")] Nothing -- STOMP 1.2 explicitly says we MUST NOT trim values

testParseConnected =  parseCheck "connected message" 
    "CONNECTED\nsession:ID:123\nheartbeat:0,0\nversion:1.0\n\n\NUL" $
    StompFrame "CONNECTED" [("session", "ID:123"), ("heartbeat", "0,0"), ("version", "1.0")] Nothing

testParseNoHeaders = parseCheck "without headers" "CONNECTED\n\n\NUL" $
    StompFrame "CONNECTED" [] Nothing
  
testParseBody = parseCheck "with body" "CONNECTED\n\nABC1\NUL" $
    StompFrame "CONNECTED" [] (Just . (fmap c2w) $ "ABC1")

testQCase1 = serializeAndParseCheck "QuckCheck failure 1" $
    StompFrame "\ACK" [("=]\b\ACK","\SUBl',\NAK"),("%\DC2?\DELK"," v"),("a\SOH","5"),("\DC3","Gn"),("\bkB`/S","\ENQ5V"),("\EMb.","1")] (Just [33,151,123,44,63,119])

prop_serializeAndParse f = (parseFrame . serializeFrame) f == Right f

newtype NonLfChar = NonLfChar { getNonLfChar :: Char } 

instance Arbitrary NonLfChar where 
    arbitrary = (NonLfChar . w2c) `fmap` (elements $ [0..9] ++ [11..127])

newtype Octet = Octet { getOctet :: Word8 }

instance Arbitrary Octet where
    arbitrary = Octet `fmap` elements [0..255]

instance Arbitrary StompFrame where
    arbitrary = do
        cmd <- listOf1 $ getNonLfChar `fmap` arbitrary
        headers <- listOf genHeader
        body <- oneof [return Nothing, Just `fmap` listOf1 ((getOctet `fmap` arbitrary) `suchThat` (/= 0))]
        return $ StompFrame cmd headers body 
      where 
            genHeader :: Gen StompHeader
            genHeader = do
              n <- listOf1 $ (getNonLfChar `fmap` arbitrary) `suchThat` (/= ':')
              v <- listOf1 $ getNonLfChar `fmap` arbitrary
              return (n, v)

