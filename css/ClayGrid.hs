{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- | Partial implementation of <https://alligator.io/css/css-grid-layout-grid-areas grid area CSS API>.
--
-- For instance, you want to generate the following CSS:
--
-- @
-- .grid1 {
--   display: grid;
--   width: max-content;
-- }
--
-- .grid3 {
--   display: grid;
--   width: max-content;
-- }
--
-- \@media (min-width: 40.0rem) {
--   .grid3 {
--     display: grid;
--     grid-template-columns: 1fr 1fr 1fr;
--     grid-gap: 1rem;
--     width: max-content;
--   }
-- }
-- @
--
-- The corresponding clay code:
--
-- @
--  ".grid1" ? do
--    display grid
--    width maxContent
--  ".grid3" ? do
--    display grid
--    width maxContent
--  query M.screen [M.minWidth (rem 40)] $ ".grid3" ? do
--    display grid
--    gridTemplateColumns [fr 1, fr 1, fr 1]
--    gridGap $ rem 1
--    width maxContent
-- @
module ClayGrid where

import Clay.Property
import Clay.Size
import Clay.Stylesheet

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Coerce (coerce)
import GHC.Exts (IsList(..))


-- | Property sets the gaps (gutters) between rows and columns.
gap :: Size a -> Css
gap = key "gap" <> key "grid-gap"

-- | Property sets the gap (gutter) between rows.
rowGap :: Size a -> Css
rowGap = key "row-gap" <> key "grid-row-gap"

-- | Property sets the gap (gutter) between columns.
columnGap :: Size a -> Css
columnGap = key "column-gap" <> key "grid-column-gap"


-- | Property defines the line names and track sizing functions of the grid columns.
gridTemplateColumns :: [Size a] -> Css
gridTemplateColumns = key "grid-template-columns" . noCommas

-- | Property defines the template for grid layout
gridTemplateAreas :: GridTemplateAreas -> Css
gridTemplateAreas = key "grid-template-areas"


gridArea :: GridArea -> Css
gridArea = key "grid-area"


newtype GridArea = GridArea Text
  deriving (IsString, Val)

newtype GridTemplateAreas = GridTemplateAreas { unGridTemplateAreas :: [[GridArea]] }

instance IsList GridTemplateAreas where
  type Item GridTemplateAreas = [GridArea]
  fromList = GridTemplateAreas
  toList = unGridTemplateAreas

instance Val GridTemplateAreas where
  value areas =
    value $
    Text.intercalate "\n" $
    fmap (Text.intercalate " ") $
    (coerce areas :: [[Text]])

