{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.Types.Context (Context(..), defaultContext) where
import Network.API.TheTVDB.HTTP
import Network.API.TheTVDB.Types.API
import Network.HTTP.Types (renderQuery, simpleQueryToQuery)
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Conduit (Manager)
import System.Directory (getAppUserDataDirectory)

-- | FIXME:
data Context = Context
  { apiKey      :: Key           -- ^ TheTVDB API key.
  , apiLang     :: String        -- ^ The metadata language to request.
  , cacheDir    :: FilePath      -- ^ Directory to store cache files in.
  , httpManager :: Maybe Manager -- ^ Optional 'Manager' for HTTP connections.
  }

instance API Context where
  fetch = fetchC

-- | Create a default 'Context' with the API key set to the given key,
-- the metadata language to set English and the cache directory set to
-- the default directory.
--
-- If you are going to be making several API calls or you don't want
-- to store the cache files/database in the default directory you
-- should create a 'Context' manually.
--
-- Furthermore, this function sets the 'http-conduit' manager to
-- Nothing which forces a new manager to be created with each API
-- call.  For better performance you probably want to share a single
-- manager across all of your API calls.
defaultContext :: Key -> IO Context
defaultContext key = do dir <- defaultDir
                        return $ Context key "en" dir Nothing

-- Base URL for the service.
apiBaseURL :: URL
apiBaseURL = "http://www.thetvdb.com"

-- Default directory used to store cache files.
defaultDir :: IO FilePath
defaultDir = getAppUserDataDirectory "thetvdb"

makeURL :: Context -> Path -> Query -> URL
--makeURL c path params = apiBaseURL ++ path q (apiKey c) (apiLang c) ++ qStr
makeURL c path params = apiBaseURL ++ path ++ qStr
  where qStr = B.unpack $ renderQuery True $ simpleQueryToQuery params

fetchC :: Context -> Path -> Query -> Disposition r -> IO (Result r)
fetchC c path params disposition =
  get (makeURL c path params) (httpManager c) disposition
