module AccessKeys
( AccessKeys(..)
, getAccessKeys
) where

import qualified Data.ByteString.Char8 as B
import Text.ParserCombinators.Parsec as P

data AccessKeys = AccessKeys {
    consumerKey     :: B.ByteString
  , consumerSecret  :: B.ByteString
  , userToken       :: B.ByteString
  , userSecret      :: B.ByteString
  } deriving (Show)

getAccessKeys :: IO AccessKeys
getAccessKeys = do
  Right keys <- parseFromFile accessKeysFile "./access-tokens"
  return keys

accessKeysFile :: P.CharParser st AccessKeys
accessKeysFile = do
  consumerKey <- accessKeysLine "consumer-key"
  consumerSecret <- accessKeysLine "consumer-secret"
  userToken <- accessKeysLine "user-token"
  userSecret <- accessKeysLine "user-secret"
  P.eof
  return $ AccessKeys consumerKey consumerSecret userToken userSecret

accessKeysLine :: String -> P.CharParser st B.ByteString
accessKeysLine key = do
  P.string key
  P.char ':'
  P.spaces
  value <- P.many $ P.noneOf "\n"
  P.many $ P.char '\n'
  return $ B.pack value
