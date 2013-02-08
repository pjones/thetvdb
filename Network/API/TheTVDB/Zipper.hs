{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Network.API.TheTVDB.Zipper
       ( Zipper()
       , zipper
       , find
       , series
       , season
       , episode
       , episodes
       , prevS
       , nextS
       , prevE
       , nextE
       ) where

import Network.API.TheTVDB.Types.API (UniqueID)
import Network.API.TheTVDB.Types.Series (Series(..))
import Network.API.TheTVDB.Types.Season (Season(..), SeasonNum)
import Network.API.TheTVDB.Types.Episode (Episode(..), EpisodeNum)
import Data.Maybe (listToMaybe)

-- Internal type synonym.
type ListZipper a = ([a], [a])

-- | A zipper type to track where you are (season, episode) in a TV
-- series.  You can move forward and backwards by seasons and
-- episodes.
data Zipper = Zipper Series (ListZipper Season) (ListZipper Episode)
              deriving (Show)

-- Internal class to find season and episodes by their numbers.
class ItemNumber a where
  itemNum :: a -> UniqueID

-- Seasons are found by seasonNumber.
instance ItemNumber Season where
  itemNum = seasonNumber

-- Episodes are found by episodeNumber.
instance ItemNumber Episode where
  itemNum = episodeNumber

-- | Create a 'Zipper' for the given 'Series'.
zipper :: Series -> Zipper
zipper s = Zipper s (seasonList s, []) eZip
  where eZip = episodeZipper $ listToMaybe (seasonList s)

-- | Find the given episode and wrap it into a 'Zipper'.
find :: Series -> SeasonNum -> EpisodeNum -> Maybe Zipper
find s sn en = do xs <- findSeason s sn
                  ys <- findEpisode xs en
                  return $ Zipper s xs ys

-- | The 'Series' inside the zipper.
series :: Zipper -> Series
series (Zipper s _ _) = s

-- | The currently focused 'Season' or 'Nothing'.
season :: Zipper -> Maybe Season
season (Zipper _ (x:_, _) _) = Just x
season _                     = Nothing

-- | The currently focused 'Episode' or 'Nothing'.
episode :: Zipper -> Maybe Episode
episode (Zipper _ _ (x:_, _)) = Just x
episode _                     = Nothing

-- | Create a list of 'Zipper's that represent the currently focused
-- episode and the episodes following the focused episode.
episodes :: Zipper -> [Zipper]
episodes   (Zipper _ _ ([],    _)) = []
episodes z@(Zipper a b (x:xs, ys)) = z:(episodes $ Zipper a b (xs, x:ys))

-- | Focus on the previous 'Season'.
prevS :: Zipper -> Maybe Zipper
prevS (Zipper _ (_,    []) _) = Nothing
prevS (Zipper a (ys, x:xs) _) = Just $ Zipper a (x:ys, xs) eZip
  where eZip = episodeZipper $ Just x

-- | Focus on the next 'Season'.
nextS :: Zipper -> Maybe Zipper
nextS (Zipper _ ([],    _) _) = Nothing
nextS (Zipper _ (_:[],  _) _) = Nothing
nextS (Zipper a (x:xs, ys) _) = Just $ Zipper a (xs, x:ys) eZip
  where eZip = episodeZipper $ listToMaybe xs

-- | Focus on the previous 'Episode'.
prevE :: Zipper -> Maybe Zipper
prevE (Zipper _ _ (_,    [])) = Nothing
prevE (Zipper a b (ys, x:xs)) = Just $ Zipper a b (x:ys, xs)

-- | Focus on the next 'Episode'.
nextE :: Zipper -> Maybe Zipper
nextE (Zipper _ _ ([],    _)) = Nothing
nextE (Zipper _ _ (_:[],  _)) = Nothing
nextE (Zipper a b (x:xs, ys)) = Just $ Zipper a b (xs, x:ys)

-- Helper function to create a ListZipper Episode.
episodeZipper :: Maybe Season -> ListZipper Episode
episodeZipper Nothing  = ([], [])
episodeZipper (Just s) = (episodeList s, [])

-- Helper function to find a matching season.
findSeason :: Series -> SeasonNum -> Maybe (ListZipper Season)
findSeason s n = breakAt (seasonList s) n

-- Helper function to find a matching episode.
findEpisode :: ListZipper Season -> EpisodeNum -> Maybe (ListZipper Episode)
findEpisode ([],  _) _ = Nothing
findEpisode (x:_, _) n = breakAt (episodeList x) n

-- Helper function.  Given a list of items and an item number creates
-- a ListZipper where the item with the given number is focused.
breakAt :: (ItemNumber a) => [a] -> UniqueID -> Maybe (ListZipper a)
breakAt xs n = if null b || itemNum (head b) /= n
               then Nothing
               else Just (b, reverse a)
  where (a, b) = break ((>= n) . itemNum) xs
