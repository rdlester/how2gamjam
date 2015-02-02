{-# LANGUAGE OverloadedStrings #-}
module AccessKeys
( AccessKeys(..)
, getAccessKeys
, getJsonKeys
, createToken
) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import Text.ParserCombinators.Parsec as P
import Web.Authenticate.OAuth
import Web.Twitter.Conduit

data AccessKeys = AccessKeys {
    consumerKey     :: B.ByteString
  , consumerSecret  :: B.ByteString
  , userToken       :: B.ByteString
  , userSecret      :: B.ByteString
  } deriving (Show)

accessKeysFromChar :: String -> String -> String -> String -> AccessKeys
accessKeysFromChar ck cs ut us = AccessKeys (B.pack ck) (B.pack cs) (B.pack ut) (B.pack us)

instance FromJSON AccessKeys where
  parseJSON (Object v) = accessKeysFromChar <$>
    v .: "consumerKey" <*>
    v .: "consumerSecret" <*>
    v .: "userToken" <*>
    v .: "userSecret"

createToken :: IO TWInfo
createToken = do
  keys <- getAccessKeys
  let app = twitterOAuth {
          oauthConsumerKey = consumerKey keys
        , oauthConsumerSecret = consumerSecret keys
        }
      user = newCredential (userToken keys) (userSecret keys)
  return $ setCredential app user def

getAccessKeys :: IO AccessKeys
getAccessKeys = do
  Right keys <- parseFromFile accessKeysFile "./access-tokens"
  return keys

getJsonKeys :: IO AccessKeys
getJsonKeys = do
  file <- BL.readFile "./access-tokens.json"
  Just keys <- return $ decode file
  return keys

accessKeysFile :: P.CharParser st AccessKeys
accessKeysFile = do
  consumerKey <- accessKeysLine "consumerKey"
  consumerSecret <- accessKeysLine "consumerSecret"
  userToken <- accessKeysLine "userToken"
  userSecret <- accessKeysLine "userSecret"
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
