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
       , Query(..)
       , API(..)
       ) where

import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as HTTP

-- | Type synonym for representing unique IDs.
type UniqueID = Integer -- TOOD: should this be somewhere else?

-- | Type synonym for representing an API key issued by TheTVDB.
type Key = String

-- | A type to represent possible errors returned from the API.
data Error
  = NetworkError String -- ^ Network/HTTP error.
  | ParseError   String -- ^ Error parsing the API response.
  deriving (Eq, Show)

-- Internal list of possible mirror types.
data MirrorType = XMLMirror | BannerMirror | ZipMirror
                deriving (Eq, Show, Enum, Bounded)

data Mirror = Mirror
  { mirrorURL   :: String
  , mirrorTypes :: [MirrorType]
  } deriving (Eq, Show)

-- | A member of the Query typeclass must define functions for
-- generating the path component of a URL and the query parameters.
class Query q where
  path      :: q -> Key -> String
  params    :: q -> HTTP.SimpleQuery
  queryType :: q -> MirrorType

-- | A member of the API typeclass must define two functions for
-- performing remote API requests, 'fetch' and 'download'.
class API a where
  -- | Perform an API query and return either an error or the body.
  fetch :: (Query q) => a -> q -> IO (Either Error L.ByteString)

  -- | Perform an API query and write the resulting body to a
  -- temporary file.  Returns either an error or the name of the file.
  download :: (Query q) => a -> q -> IO (Either Error FilePath)
