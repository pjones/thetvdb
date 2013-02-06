{-

This file is part of the Haskell package thetvdb. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/thetvdb/LICENSE. No part of
themoviedb package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Network.API.TheTVDB.XML (content, convert, nullWrap, maybeDate) where
import Text.XML (Name)
import Text.XML.Cursor (($/), (&//))
import qualified Text.XML.Cursor as XC
import qualified Data.Text as T
import Safe (readDef)
import System.Locale (defaultTimeLocale)
import Data.Time (Day(..), parseTime)

-- Fetch the content of the element with the given name.
content :: XC.Cursor -> Name -> T.Text
content c n = T.concat $ c $/ XC.element n &// XC.content

-- Convert the content of the given element to some type.
convert :: (Read a) => XC.Cursor -> a -> Name -> a
convert c def n = readDef def $ T.unpack $ content c n

-- Wrap some text into a Maybe depending on whether it's null or not.
nullWrap :: XC.Cursor -> Name -> Maybe T.Text
nullWrap c n = if T.null v then Nothing else Just v
  where v = content c n

-- Try to parse the date from an XML node's content.
maybeDate :: XC.Cursor -> Name -> Maybe Day
maybeDate c n = nullWrap c n >>= parse
  where parse t = parseTime defaultTimeLocale "%Y-%m-%d" (T.unpack t)
