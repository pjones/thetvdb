{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.Search where
import Network.API.TheTVDB.Types.Series (Series(..))
import Network.API.TheTVDB.Types.Config (Config())
import Network.API.TheTVDB.Types.API (Error(..))
import Data.Text (pack)

-- Let's be honest, we need a better XML parser library for Haskell,
-- something like Aeson would be great.
import Text.XML.HXT.Core

-- TODO: Move this into an XML parsing file.
-- parseSeriesXML :: ArrowXml cat =>
--                   cat (Data.Tree.NTree.TypeDefs.NTree XNode) Series
parseSeriesXML = atTag "Series" >>>
  proc x -> do
    sID   <- textAt "seriesid"   -< x
    sIMDB <- textAt "IMDB_ID"    -< x
    sName <- textAt "SeriesName" -< x
    sOver <- textAt "Overview"   -< x
    returnA -< Series
      { seriesID       = read sID
      , seriesIMDB     = pack sIMDB
      , seriesName     = pack sName
      , seriesOverview = pack sOver
      }
  where atTag  tag = deep (isElem >>> hasName tag)
        textAt tag = atTag tag >>> getChildren >>> getText

type Query = String

search :: Config -> Query -> IO [Series]
search c q = undefined

searchErr :: Config -> Query -> IO (Either Error [Series])
searchErr c q = undefined
