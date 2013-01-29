{-# LANGUAGE FlexibleContexts #-}

{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.HTTP (URL, get) where
import Network.API.TheTVDB.Types.API
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Status as Status
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadThrow, MonadUnsafeIO,
                                     ResourceT, runResourceT)
type URL = String

runWithManager :: ( MonadIO m
                  , MonadBaseControl IO m
                  , MonadThrow m
                  , MonadUnsafeIO m
                  ) => Maybe HTTP.Manager -> (HTTP.Manager -> ResourceT m a) -> m a
runWithManager manager f =
  case manager of
    Just m  -> runResourceT $ f m
    Nothing -> HTTP.withManager f

get :: URL -> Maybe HTTP.Manager -> IO (APIError L.ByteString)
get url manager =
  do request <- HTTP.parseUrl url
     runWithManager manager $ \m -> do
       response <- HTTP.httpLbs request m
       return $ if Status.statusIsSuccessful $ HTTP.responseStatus response
                  then Right $ HTTP.responseBody response
                  else Left  $ NetworkError "API HTTP error"
