{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.Search (SearchTerm, search, searchErr) where
import Network.API.TheTVDB.Types.API
import Network.API.TheTVDB.Types.Series (Series(..))
import Text.XML.Cursor (Cursor, fromDocument, element, content, ($/), (&//))
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Text.XML as X

type SearchTerm = String

search :: (API api) => api -> SearchTerm -> IO [Series]
search api term = do r <- searchErr api term
                     return $ either (const []) id r

searchErr :: (API api) => api -> SearchTerm -> IO (Result [Series])
searchErr api term = fetch api path params parse
  where path   = "/api/GetSeries.php"
        params = [(C.pack "seriesname", C.pack $ term)]

parse :: Disposition [Series]
parse = do doc <- X.sinkDoc X.def
           return $ map parseSeries (fromDocument doc $/ element "Series")

parseSeries :: Cursor -> Series
parseSeries c = Series
  { seriesID       = read $ T.unpack (extract "id")
  , seriesIMDB     = extract "IMDB_ID"
  , seriesName     = extract "SeriesName"
  , seriesOverview = extract "Overview"
  }
  where extract n = T.concat $ c $/ element n &// content
