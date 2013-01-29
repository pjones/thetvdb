{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.Types.Context () where
import Network.API.TheTVDB.HTTP
import Network.API.TheTVDB.Types.API
import Network.API.TheTVDB.Mirrors
import Network.HTTP.Types (renderQuery, simpleQueryToQuery)
import Data.Random.Extras (choice)
import Data.Random.RVar (runRVar)
import Data.Random.Source.DevRandom (DevRandom(..))
import Network.API.TheTVDB.Types.Config (Config(..))
import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

-- Information used across API transactions.
data Context = Context
  { config      :: Config     -- ^ API configuration information.
  , apiMirrors  :: [Mirror]   -- ^ List of API mirror servers.
  }

instance API Context where
  fetch    = fetchWithContext
  download = downloadWithContext

-- | Create a Context which is used in all of the API calls.
makeContext :: Config -> IO (APIError Context)
makeContext cfg = undefined

-- Returns a list of valid mirrors for the given query.
mirrorsForQuery :: (Query a) => Context -> a -> [Mirror]
mirrorsForQuery context query = filter validMirror (defaultMirror:mirrors)
  where mirrors       = apiMirrors context
        validMirror m = queryType query `elem` mirrorTypes m

-- Make a URL for the given request using an appropriate mirror.
makeURL :: (Query a) => Context -> a -> IO String
makeURL context query = liftM url mirror
  where mirror = runRVar (choice $ mirrorsForQuery context query) DevURandom
        url m  = mirrorURL m ++ path query (apiKey $ config context) ++ qStr
        qStr   = B.unpack $ renderQuery True (simpleQueryToQuery $ params query)

fetchWithContext :: (Query a) => Context -> a -> IO (APIError L.ByteString)
fetchWithContext context query = do url <- makeURL context query
                                    get url $ httpManager (config context)

downloadWithContext :: (Query a) => Context -> a -> IO (Either Error FilePath)
downloadWithContext context query = undefined
