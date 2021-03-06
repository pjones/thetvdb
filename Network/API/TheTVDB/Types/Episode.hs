{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Network.API.TheTVDB.Types.Episode (Episode(..), EpisodeID, EpisodeNum) where
import Data.Text (Text)
import Data.Time (Day(..))
import Network.API.TheTVDB.Types.API (UniqueID)

type EpisodeID = UniqueID
type EpisodeNum = UniqueID

data Episode = Episode
  { episodeID       :: EpisodeID
  , episodeNumber   :: EpisodeNum
  , episodeName     :: Text
  , episodeOverview :: Text
  , episodeDate     :: Maybe Day
  } deriving (Eq, Show)
