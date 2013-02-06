{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.HTTP (get) where
import Control.Monad (liftM)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Network.API.TheTVDB.Types.API
import qualified Data.Conduit as C
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types.Status as HS
import qualified Data.Text as T

-- Perform an HTTP GET and send the resulting body to the sink given
-- in the 'Disposition' argument.
get :: URL -> Maybe H.Manager -> Disposition r -> IO (Result r)
get url manager pipe =
  do request <- H.parseUrl $ T.unpack url
     runWithManager manager $ \manager' -> do
       response <- H.http request manager'
       if HS.statusIsSuccessful $ H.responseStatus response
         then liftM Right (H.responseBody response C.$$+- pipe)
         else return . Left $ NetworkError errorMsg
  where errorMsg = "the server did not respond with the requested information"

-- Internal function to ensure that 'runResourceT' is called.
runWithManager :: Maybe H.Manager -> (H.Manager -> ResourceT IO a) -> IO a
runWithManager manager f = case manager of
  Just m  -> runResourceT $ f m
  Nothing -> H.withManager f
