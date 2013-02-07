{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Main where
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)
import qualified Data.Text as T

import Network.API.TheTVDB (
  Series(..), Season(..),
  loadKeyMaybe, defaultContext, search, fetch)

putSeriesSimple :: Series -> IO ()
putSeriesSimple s = printf "%8d: %s\n" sid name
  where sid  = seriesID s
        name = T.unpack (seriesName s)

putSeriesDetailed :: Series -> IO ()
putSeriesDetailed s =
  do putSeriesSimple s
     putStrLn $ "Seasons: "  ++ (show . length) seasons
     putStrLn $ "Episodes: " ++ show episodes
     maybe (return ()) (putStrLn . T.unpack) $ seriesPosterURL s
     putStrLn "-- "
     putStrLn $ T.unpack $ seriesOverview s
  where seasons  = filter realSeason (seasonList s)
        episodes = sum $ map (length . episodeList) seasons

realSeason :: Season -> Bool
realSeason = (/= 0) . seasonNumber

main :: IO ()
main = do args <- getArgs
          key <- loadKeyMaybe >>= maybe missingKey return
          ctx <- defaultContext key
          case args of
            ["key"]          -> putStrLn key
            ["search", term] -> search ctx term >>= mapM_ putSeriesSimple
            ["fetch", sid]   -> fetch ctx (read sid) >>= putSeriesDetailed
            _                -> putStrLn usage >> exitFailure
  where missingKey = fail "failed to load your API key"
        usage = "Usage: tvdb {search QUERY|fetch ID|key}\n\n" ++
                "Description:\n" ++
                "       search: find a series on TheTVDB\n" ++
                "        fetch: retrieve info on a series\n" ++
                "          key: show your API key\n\n" ++
                "Example:\n" ++
                "       tvdb search \"how I met your mother\"\n" ++
                "       tvdb fetch 75760\n"
