{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Network.API.TheTVDB.Types.Series (Series(..)) where
import Data.Text (Text())
import Network.API.TheTVDB.Types.API (UniqueID)
import Network.API.TheTVDB.Types.Season (Season)

data Series = Series
  { seriesID        :: UniqueID
  , seriesIMDB      :: Text
  , seriesName      :: Text
  , seriesOverview  :: Text
  , seriesPosterURL :: Maybe Text
  , seasonList      :: [Season]
  } deriving (Eq, Show)
