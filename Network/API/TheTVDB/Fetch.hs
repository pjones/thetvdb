{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Network.API.TheTVDB.Fetch
       (Network.API.TheTVDB.Fetch.fetch, fetchErr) where

import Data.Maybe (fromMaybe)
import Network.API.TheTVDB.Types.Context (posterBaseURL)
import Network.API.TheTVDB.Types.Episode (Episode(..))
import Network.API.TheTVDB.Types.Season (Season(..))
import Network.API.TheTVDB.Types.Series (Series(..))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.API.TheTVDB.Types.API as API
import qualified Text.XML as X

import Network.API.TheTVDB.Types.API
  (API(..), Query(..), Result, UniqueID, Disposition)

import Text.XML.Cursor
  (Cursor, fromDocument, element, content, parent, ($|), ($/), (&/), (&//))

newtype Fetch = Fetch {fetchSeriesID :: UniqueID}

instance Query Fetch where
  path q key lang = "/api/" ++ key ++ "/series/" ++ show (fetchSeriesID q)
                    ++ "/all/" ++ lang ++ ".xml"
  params _ = []

fetch :: (API api) => api -> UniqueID -> IO Series
fetch api sid = do result <- fetchErr api sid
                   either (fail . show) return result

fetchErr :: (API api) => api -> UniqueID -> IO (Result Series)
fetchErr api sid = API.fetch api (Fetch sid) parse

parse :: Disposition Series
parse = do doc <- X.sinkDoc X.def
           let cursor      = fromDocument doc
               seriesNodes = cursor $/ element "Series"
           if length seriesNodes == 1
             then return $ parseSeries (head seriesNodes)
             else error "WTF" -- FIXME:

parseSeries :: Cursor -> Series
parseSeries c = Series
  { seriesID        = read $ T.unpack (extract "id")
  , seriesIMDB      = extract "IMDB_ID"
  , seriesName      = extract "SeriesName"
  , seriesOverview  = extract "Overview"
  , seriesPosterURL = Just $ T.concat [T.pack posterBaseURL, extract "poster"]
  , seasonList      = parseSeasons c
  }
  where extract n = T.concat $ c $/ element n &// content

parseSeasons :: Cursor -> [Season]
parseSeasons c = M.elems $ foldr collapse M.empty episodes
  where episodes = map parseEpisode (c $| parent &/ element "Episode")
        collapse (s, e) m = insert (find s m) e m
        find s m = fromMaybe s $ M.lookup (seasonNumber s) m
        insert s e = M.insert (seasonNumber s) (cons s e)
        cons s e = s {episodeList = e : episodeList s}

parseEpisode :: Cursor -> (Season, Episode)
parseEpisode c = (season, episode)
  where extract n = T.concat $ c $/ element n &// content
        season  = Season { seasonID = read $ T.unpack (extract "seasonid")
                         , seasonNumber = read $ T.unpack (extract "SeasonNumber")
                         , episodeList = []
                         }
        episode = Episode { episodeID = read $ T.unpack (extract "id")
                          , episodeNumber   = read $ T.unpack (extract "EpisodeNumber")
                          , episodeName     = extract "EpisodeName"
                          , episodeOverview = extract "Overview"
                          , episodeDate     = Nothing
                          }
