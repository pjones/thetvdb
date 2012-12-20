{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.Types.Config (Key, Config(..), defaultConfig) where
import Network.API.TheTVDB.Types.API (Mirror, Key)
import Control.Applicative
import Control.Monad (liftM)
import Network.HTTP.Conduit (Manager)
import System.Directory (getAppUserDataDirectory)

-- | Data type to hold configuration information for the API.
data Config = Config
  { apiKey      :: Key           -- ^ TheTVDB API key.
  , cacheDir    :: FilePath      -- ^ Directory to store cache files in.
  , httpManager :: Maybe Manager -- ^ Optional 'Manager' for HTTP connections.
  , apiMirrors  :: [Mirror]      -- ^ List of API mirror servers.
  }
  
-- Default directory used to store cache files.
defaultDir :: IO FilePath
defaultDir = getAppUserDataDirectory "thetvdb"

-- | Create a default 'Config' with the API key set to the given key
-- and the cache directory set to the default directory.
--
-- If you are going to be making several API calls or you don't want
-- to store the cache files/database in the default directory you
-- should create a 'Config' manually.
--
-- Furthermore, this function sets the 'http-conduit' manager to
-- Nothing which forces a new manager to be created with each API
-- call.  For better performance you probably want to share a single
-- manager across all of your API calls.
defaultConfig :: Key -> IO Config
defaultConfig key = do dir <- defaultDir
                       return $ Config key dir Nothing []

