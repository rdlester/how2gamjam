{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Char8 as B
import Text.ParserCombinators.Parsec
import Web.Authenticate.OAuth
import Web.Twitter.Conduit

main :: IO ()
main = do
  keys <- getAccessKeys
  print keys

createToken :: AccessKeys -> TWInfo
createToken keys = setCredential app user def
  where app = twitterOAuth {
            oauthConsumerKey = consumerKey keys
          , oauthConsumerSecret = consumerSecret keys
          }
        user = Credential [
            ("oauth_token", userToken keys)
          , ("oauth_token_secret", userSecret keys)
          ]

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

accessKeysFile :: CharParser st AccessKeys
accessKeysFile = do
  consumerKey <- accessKeysLine "consumer-key"
  consumerSecret <- accessKeysLine "consumer-secret"
  userToken <- accessKeysLine "user-token"
  userSecret <- accessKeysLine "user-secret"
  eof
  return $ AccessKeys consumerKey consumerSecret userToken userSecret

accessKeysLine :: String -> CharParser st B.ByteString
accessKeysLine key = do
  string key
  char ':'
  spaces
  value <- many $ noneOf "\n"
  many $ char '\n'
  return $ B.pack value
