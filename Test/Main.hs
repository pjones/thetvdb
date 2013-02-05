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
import Network.API.TheTVDB.Types.API (API(..))
import Network.API.TheTVDB.Types.Series (Series(..))
import System.Exit (exitFailure)
import Test.HUnit (Test(..), Counts(..), runTestTT,
                   assertFailure, assertEqual, assertBool)

-- ============================================================================
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
-- Why can't this be automatic?
unitTests :: Test
unitTests = TestList
  [ TestLabel "Can parse search results" goodSearchTest
  ]

-- ============================================================================
main :: IO ()
main = do counts <- runTestTT unitTests
          let bad = errors counts + failures counts
          when (bad > 0) exitFailure
