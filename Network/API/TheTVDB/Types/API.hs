{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}
module Network.API.TheTVDB.Types.API
       ( UniqueID
       , Key
       , Error(..)
       , API(..)
       , Result
       , URL
       , Path
       , Query(..)
       , Disposition
       ) where

import qualified Network.HTTP.Types as H
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString as S
import qualified Data.Conduit as C

-- | Type synonym for representing unique IDs.
type UniqueID = Integer -- TOOD: should this be somewhere else?

-- | Type synonym for representing an API key issued by TheTVDB.
type Key = String

-- | FIXME:
type Language = String

-- | A type to represent possible errors returned from the API.
data Error
  = NetworkError String -- ^ Network/HTTP error.
  | ParseError   String -- ^ Error parsing the API response.
  deriving (Eq)

instance Show Error where
  show (NetworkError e) = msg ++ e
    where msg = "An error occurred while communicating with TheTVDB.com: "

  show (ParseError e) = msg ++ e
    where msg = "TheTVDB.com responded with an invalid document: "

-- FIXME:
type Result = Either Error
type URL = String
type Path = String
type Disposition r = C.Sink S.ByteString (ResourceT IO) r

-- | A member of the Query typeclass must define functions for
-- generating the path component of a URL and the query parameters.
class Query q where
  path        :: q -> Key -> Language -> Path
  params      :: q -> H.SimpleQuery

-- | A member of the API typeclass must define functions for
-- performing remote API requests.
class API api where
  -- | Perform an API query and return either an error or the body.
  fetch :: Query query => api -> query -> Disposition r -> IO (Result r)
