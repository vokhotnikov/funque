module Funque.Stomp.Parser where

import Data.Word(Word8)
import Data.ByteString(ByteString)
import Data.ByteString.Internal(c2w,w2c)
import Text.Parsec((<?>), many, many1, optionMaybe, parse, ParseError)
import Text.Parsec.ByteString
import Text.Parsec.Prim(tokenPrim)
import Text.Parsec.Pos (updatePosChar, updatePosString)

type StompHeader = (String, String)

type StompBody = [Word8]

data StompFrame = StompFrame String [StompHeader] (Maybe StompBody) deriving (Show, Eq)

parseFrame :: ByteString -> Either ParseError StompFrame 
parseFrame = parse p_frame "STOMP stream"

serializeFrame :: StompFrame -> ByteString
serializeFrame f = undefined

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
p_string s = p_str s <?> "literal '" ++ s ++ "'"
  where
    p_str [] = satisfy (const True) >> return ""
    p_str (x:xs) = do
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

