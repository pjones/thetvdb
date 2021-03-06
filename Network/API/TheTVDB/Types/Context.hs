{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.Types.Context
       (Context(..), defaultContext, posterBaseURL) where

import Network.API.TheTVDB.Types.API
import Network.HTTP.Types (renderQuery, simpleQueryToQuery)
import qualified Data.ByteString.Char8 as B
import System.Directory (getAppUserDataDirectory)
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HS
import qualified Data.Conduit as C
import Control.Monad (liftM)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

-- | FIXME:
data Context = Context
  { apiKey :: Key
    -- ^ TheTVDB API key.

  , apiLang :: Language
    -- ^ The language to request from the API.  The default is English
    -- (en).  The language parameter is given directly to TheTVDB API
    -- which should then return text (e.g. names and overviews) in the
    -- requested language.

  , cacheDir :: FilePath
    -- ^ Directory to store cache files in.  Not currently used.

  , httpTimeout :: Maybe Int
    -- ^ Optional request timeout in microseconds.  This is passed to
    -- the @http-conduit@ package.  The default is 60 seconds (the API
    -- can be really, really slow).  'Nothing' means disable the
    -- timeout and wait forever.

  , httpManager :: Maybe H.Manager
    -- ^ Optional 'Manager' for HTTP connections.
  }

instance API Context where
  fetch = fetchC

-- | Create a default 'Context' with the API key set to the given key,
-- the metadata language to set English and the cache directory set to
-- the default directory.
--
-- If you are going to be making several API calls or you don't want
-- to store the cache files in the default directory you should create
-- a 'Context' manually (or update the one from this function).
--
-- Furthermore, this function sets the 'http-conduit' manager to
-- 'Nothing' which forces a new manager to be created with each API
-- call.  For better performance you probably want to share a single
-- manager across all of your API calls.
defaultContext :: Key -> IO Context
defaultContext key = do dir <- defaultDir
                        return $ Context key defLang dir defTO Nothing
  where defLang = "en"
        defTO   = Just 60000000

-- Base URL for the service.
apiBaseURL :: URL
apiBaseURL = "http://www.thetvdb.com"

-- Base URL for series posters
posterBaseURL :: URL
posterBaseURL = T.append apiBaseURL "/banners/"

-- Default directory used to store cache files.
defaultDir :: IO FilePath
defaultDir = getAppUserDataDirectory "thetvdb"

-- Helper function to create a URL from a Query.
makeURL :: Query query => Context -> query -> URL
makeURL c q  = T.concat [apiBaseURL, path q (apiKey c) (apiLang c), qStr]
  where qStr = T.pack $ B.unpack $ renderQuery True $ simpleQueryToQuery (params q)

-- Use the HTTP module to make a remote HTTP GET call for the API.
fetchC :: Query query => Context -> query -> Disposition r -> IO (Result r)
fetchC c q pipe =
  do req <- H.parseUrl $ T.unpack $ (makeURL c q)
     let req' = req {H.responseTimeout = httpTimeout c}
     runWithManager (httpManager c) $ \manager -> do
       response <- H.http req' manager
       if HS.statusIsSuccessful $ H.responseStatus response
         then liftM Right (H.responseBody response C.$$+- pipe)
         else return . Left $ NetworkError errorMsg
  where errorMsg = "the server did not respond with the requested information"

-- Internal function to ensure that 'runResourceT' is called.
runWithManager :: Maybe H.Manager -> (H.Manager -> ResourceT IO a) -> IO a
runWithManager manager f = case manager of
  Just m  -> runResourceT $ f m
  Nothing -> H.withManager f
