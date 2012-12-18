{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.Types.Config where
import Network.API.TheTVDB.Types.API
import System.Directory (getAppUserDataDirectory)
import Control.Applicative
import Control.Monad (liftM)

-- | Data type to hold configuration information for the API.
data Config = Config
  { apiKey   :: Key       -- ^ TheTVDB API key.
  , cacheDir :: FilePath  -- ^ Directory to store cache files in.
  }

-- | Different ways of creating a 'Config' using 'mkConfig'.
data ConfigDefaults
  = DefaultAll                 -- ^ Load all data from the system or fail.
  | WithKey Key                -- ^ Use the given key and the default cacheDir.
  | WithKeyAndDir Key FilePath -- ^ Don't use any default values.
  
-- | Create a 'Config'.  Keep in mind that if you give this function
-- 'DefaultAll' and the API key can't be found in the environment or
-- the file system 'fail' will be called.  To avoid this use 'WithKey'
-- or 'WithKeyAndDir'.  You can load a key manually using
-- 'loadKeyMaybe'.
mkConfig :: ConfigDefaults -> IO Config
mkConfig DefaultAll              = Config <$> loadKey <*> defaultDir
mkConfig (WithKey key)           = liftM (Config key) defaultDir
mkConfig (WithKeyAndDir key dir) = return $ Config key dir

-- | Load a key from the environment or file system.
loadKeyMaybe :: IO (Maybe Key)
loadKeyMaybe = undefined
                                   
-- Load a key from the environment or file system.
loadKey :: IO Key
loadKey = loadKeyMaybe >>= maybe err return
  where err = fail "failed to load API keys for TheTVDB"

-- Default directory used to store cache files.
defaultDir :: IO FilePath
defaultDir = getAppUserDataDirectory "thetvdb"
