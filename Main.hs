{-# LANGUAGE OverloadedStrings #-}
import AccessKeys
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import Data.Conduit
import qualified Data.Conduit.List as CL
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens

main :: IO ()
main = do
  keys <- getAccessKeys
  withManager $ \mgr -> do
    let source = sourceWithMaxId (createToken keys) mgr homeTimeline
        limiter = CL.isolate 60
        sink = CL.mapM_ printStatus
        printStatus status = liftIO $ print $ status ^. statusText
    source $= limiter $$ sink


createToken :: AccessKeys -> TWInfo
createToken keys = setCredential app user def
  where app = twitterOAuth {
            oauthConsumerKey = consumerKey keys
          , oauthConsumerSecret = consumerSecret keys
          }
        user = newCredential (userToken keys) (userSecret keys)

