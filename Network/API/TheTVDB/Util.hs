{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.Util (loadKeyMaybe, loadKey) where
import Network.API.TheTVDB.Types.Config (Key)

-- | Load a key from the environment or file system.
loadKeyMaybe :: IO (Maybe Key)
loadKeyMaybe = undefined

-- | Load a key from the environment or file system and fail if the
-- key can't be loaded from any of the sources mentioned in
-- 'loadKeyMaybe'.
loadKey :: IO Key
loadKey = loadKeyMaybe >>= maybe err return
  where err = fail "failed to load API keys for TheTVDB"
