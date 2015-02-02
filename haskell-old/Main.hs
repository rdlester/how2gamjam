{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
import AccessKeys
import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.List as CL
import Network.HTTP.Conduit
import System.IO
import Web.Authenticate.OAuth
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens

main :: IO ()
main = do
  login <- createToken
  saveCorpus login

saveTimeline :: TWInfo -> IO ()
saveTimeline login = saveTweets login "timeline.txt" homeTimeline corpusSink

saveCorpus :: TWInfo -> IO ()
saveCorpus login =
  withFile "corpus.txt" AppendMode $ \h ->
    withManager $ \mgr -> call' login mgr (searchTweets "#gamedev") $= CL.isolate 1 $$ searchSink h

saveTweets :: (FromJSON b, AsStatus b, HasMaxIdParam (APIRequest a [b])) =>
  TWInfo -> FilePath -> APIRequest a [b] -> (Handle -> Sink b (ResourceT IO) ()) -> IO ()
saveTweets login path request sink =
  withFile path AppendMode $ \h ->
    withManager $ \mgr -> sourceWithMaxId login mgr request $= CL.isolate 60 $$ sink h

saveHandler :: SomeException -> IO ()
saveHandler _ = putStrLn "corpus collection ended"

corpusSink :: Handle -> Sink Status (ResourceT IO) ()
corpusSink h = CL.mapM_ $ \status -> liftIO $ hPrint h $ status^.statusText

searchSink :: Handle -> Sink (SearchResult [SearchStatus]) (ResourceT IO) ()
searchSink h = CL.mapM_ $ \result -> liftIO $ mapM_ (\status -> hPrint h $ status^.searchStatusText) $ result^.searchResultStatuses
