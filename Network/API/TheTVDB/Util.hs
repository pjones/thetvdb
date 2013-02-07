{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.Util (loadKeyMaybe, loadKey) where
import Control.Exception (catch)
import Control.Monad (liftM, msum)
import Data.Char (isSpace)
import Network.API.TheTVDB.Types.API (Key)
import Prelude hiding (catch)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (fileExist)
import System.Directory (getHomeDirectory)

-- Fetch the value of an environment variable without an exception.
getEnvMaybe :: String -> IO (Maybe String)
getEnvMaybe n = env `catch` err
  where env   = liftM Just $ getEnv n
        err e = if isDoesNotExistError e
                  then return Nothing
                  else ioError e

-- Expand @~@ at the front of a file path.
expandFile :: FilePath -> IO FilePath
expandFile ('~':rest) = liftM (++ rest) getHomeDirectory
expandFile dir        = return dir

-- Return the contents of a file if it exists, otherwise Nothing.
readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe n = do realName <- expandFile n
                     exists   <- fileExist realName
                     if exists
                       then liftM Just (readFirstLine realName)
                       else return Nothing
  where readFirstLine = liftM skipSpace . readFile
        skipSpace     = filter $ not . isSpace


-- | Load a key from the environment or file system.
loadKeyMaybe :: IO (Maybe Key)
loadKeyMaybe = liftM msum . sequence $ [env, xdgConfig, config, home]
  where env       = getEnvMaybe "TVDB_KEY"
        config    = readFileMaybe "~/.config/tvdbkey"
        home      = readFileMaybe "~/.tvdbkey"
        xdgConfig = do dir <- getEnvMaybe "XDG_CONFIG_HOME"
                       case dir of
                         Nothing -> return Nothing
                         Just d  -> readFileMaybe (d ++ "/tvdbkey")

-- | Load a key from the environment or file system and fail if the
-- key can't be loaded from any of the sources mentioned in
-- 'loadKeyMaybe'.
loadKey :: IO Key
loadKey = loadKeyMaybe >>= maybe err return
  where err = fail "failed to load API keys for TheTVDB"
