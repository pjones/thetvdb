{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Main where
import Control.Monad (liftM, when)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$))
import Data.Conduit.Binary (sourceFile)
import Network.API.TheTVDB.Search (searchErr)
import Network.API.TheTVDB.Fetch (fetchErr)
import Network.API.TheTVDB.Types.API (API(..))
import Network.API.TheTVDB.Types.Series (Series(..))
import Network.API.TheTVDB.Types.Season (Season(..))
import System.Exit (exitFailure)
import Test.HUnit (Test(..), Counts(..), runTestTT,
                   assertFailure, assertEqual, assertBool)

-- ============================================================================
-- Don't go to the network for requests, load XML files from the disk.
newtype TestXML = TestXML {xmlFilePath :: FilePath}

instance API TestXML where
  fetch a _ d = liftM Right $ runResourceT $ sourceFile (xmlFilePath a) $$ d

-- ============================================================================
goodSearchTest :: Test
goodSearchTest = TestCase $ do
  result <- searchErr (TestXML "Test/search.xml") "mother"
  case result of
    Left e  -> assertFailure $ show e
    Right ss -> do
      assertEqual "results size" 13 (length ss)
      let s = head ss
      assertEqual "first series ID" 178331 $ seriesID s
      assertEqual "first series name" "Mother" $ seriesName s
      assertEqual "first series IMDB" "tt1632065" $ seriesIMDB s

-- ============================================================================
goodFetchTest :: Test
goodFetchTest = TestCase $ do
  result <- fetchErr (TestXML "Test/series.xml") 0
  case result of
    Left  e -> assertFailure $ show e
    Right s -> do
      assertEqual "series name" "How I Met Your Mother" $ seriesName s
      assertEqual "series ID" 75760 $ seriesID s
      assertEqual "number of seasons" 9 $ length (seasonList s)
      assertEqual "first season ID" 23219 $ seasonID (head $ seasonList s)
      assertEqual "last season ID" 496187 $ seasonID (last $ seasonList s)
      mapM_ checkCount (zippedEpisodes s)
  where checkCount (e, a) = assertEqual "episode count" e a
        zippedEpisodes s = zip expectEpisodeCounts (actualEpisodeCounts s)
        actualEpisodeCounts s = map (length . episodeList) (seasonList s)
        expectEpisodeCounts = [10, 22, 22, 20, 24, 24, 24, 24, 17]

-- ============================================================================
-- Why can't this be automatic?
unitTests :: Test
unitTests = TestList
  [ TestLabel "Can parse search results" goodSearchTest
  , TestLabel "Can parse series data" goodFetchTest
  ]

-- ============================================================================
main :: IO ()
main = do counts <- runTestTT unitTests
          let bad = errors counts + failures counts
          when (bad > 0) exitFailure
