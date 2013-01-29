{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

-- http://thetvdb.com/wiki/index.php/API:mirrors.xml
module Network.API.TheTVDB.Mirrors (defaultMirror, fetchMirrors) where
import Network.API.TheTVDB.Types.API
import Network.API.TheTVDB.Types.Config
import Network.API.TheTVDB.HTTP
import qualified Data.ByteString.Lazy as L

-- The primary API server and default mirror.
defaultMirror :: Mirror
defaultMirror =  Mirror
  { mirrorURL   = "http://www.thetvdb.com"
  , mirrorTypes = enumFrom minBound
  }

fetchMirrors :: Config -> IO (APIError L.ByteString)
fetchMirrors cfg = xml
  where xml = get url $ httpManager cfg
        url = mirrorURL defaultMirror ++ "/api/" ++ apiKey cfg ++ "/mirrors.xml"
