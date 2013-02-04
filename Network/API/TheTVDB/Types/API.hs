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
       , MirrorType(..)
       , Mirror(..)
       , Error(..)
       , API(..)
       , Result
       , URL
       , Path
       , Query
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
  deriving (Eq, Show)

-- errroMsg = "An error occurred while communicating with TheTVDB.com"

-- FIXME:
type Result = Either Error
type URL = String
type Path = String
type Query = H.SimpleQuery

-- Sink i m r = Pipe i i Void () m r
--              Pipe l Event o u m a
--              Pipe l ByteString Event r m r
--         data Pipe l i o u m r
-- type Disposition o r = C.Pipe S.ByteString S.ByteString o r (ResourceT IO) r
type Disposition r = C.Sink S.ByteString (ResourceT IO) r

-- Internal list of possible mirror types.
data MirrorType = XMLMirror | BannerMirror | ZipMirror
                deriving (Eq, Show, Enum, Bounded)

data Mirror = Mirror
  { mirrorURL   :: String
  , mirrorTypes :: [MirrorType]
  } deriving (Eq, Show)

-- | A member of the Query typeclass must define functions for
-- generating the path component of a URL and the query parameters.
-- class Query q where
--   path        :: q -> Key -> Language -> Path
--   params      :: q -> H.SimpleQuery
--   disposition :: q -> Disposition r

-- | A member of the API typeclass must define two functions for
-- performing remote API requests, 'fetch' and 'download'.
class API api where
  -- | Perform an API query and return either an error or the body.
  fetch :: api -> Path -> H.SimpleQuery -> Disposition r -> IO (Result r)
