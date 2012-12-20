{-# LANGUAGE FlexibleContexts #-}

{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.HTTP () where
import Network.API.TheTVDB.Types.API
import Network.API.TheTVDB.Types.Config (Config(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Types (renderQuery, simpleQueryToQuery)
import qualified Network.HTTP.Types.Status as Status
import Data.Random.Extras (choice)
import Data.Random.RVar (runRVar)
import Data.Random.Source.DevRandom (DevRandom(..))
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadThrow, MonadUnsafeIO,
                                     ResourceT, runResourceT)

-- The primary API server and default mirror.
defaultMirror :: Mirror
defaultMirror = Mirror { mirrorURL   = "http://www.thetvdb.com"
                       , mirrorTypes = enumFrom minBound
                       }

-- Returns a list of valid mirrors for the given query.
mirrorsForQuery :: (Query a) => Config -> a -> [Mirror]
mirrorsForQuery cfg query = if null mirrors then [defaultMirror] else mirrors
  where mirrors       = filter validMirror $ apiMirrors cfg
        validMirror m = queryType query `elem` mirrorTypes m

-- Make a URL for the given request using an appropriate mirror.
makeURL :: (Query a) => Config -> a -> IO String
makeURL cfg query = liftM url mirror
  where mirror = runRVar (choice $ mirrorsForQuery cfg query) DevURandom
        url m  = mirrorURL m ++ path query (apiKey cfg) ++ qStr
        qStr   = B.unpack $ renderQuery True (simpleQueryToQuery $ params query)

runWithManager :: ( MonadIO m
                  , MonadBaseControl IO m
                  , MonadThrow m
                  , MonadUnsafeIO m
                  ) => Config -> (HTTP.Manager -> ResourceT m a) -> m a
runWithManager cfg f =
  case httpManager cfg of
    Just m  -> runResourceT $ f m
    Nothing -> HTTP.withManager f

fetchWithConfig :: (Query a) => Config -> a -> IO (Either Error L.ByteString)
fetchWithConfig cfg query =
  do url <- makeURL cfg query
     request <- HTTP.parseUrl url
     runWithManager cfg $ \manager -> do
       response <- HTTP.httpLbs request manager
       return $ if Status.statusIsSuccessful $ HTTP.responseStatus response
                  then Right $ HTTP.responseBody response
                  else Left  $ NetworkError "API HTTP error"

downloadWithConfig :: (Query a) => Config -> a -> IO (Either Error FilePath)
downloadWithConfig cfg query = undefined

instance API Config where
  fetch    = fetchWithConfig
  download = downloadWithConfig
