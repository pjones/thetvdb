{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB
       ( Key
       , Context(..)
       , defaultContext
       , Series(..)
       , searchErr
       , search
       ) where

import Network.API.TheTVDB.Types.Context
import Network.API.TheTVDB.Types.API
import Network.API.TheTVDB.Types.Series
import Network.API.TheTVDB.Search
