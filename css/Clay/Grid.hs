{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
module Clay.Grid where

import Data.Function ((&))

import Clay hiding ((&))
import Clay.Stylesheet hiding ((&))

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Coerce
import GHC.Exts (IsList(..))

-- lib
gridTemplateAreas :: GridTemplateAreas -> Css
gridTemplateAreas = key "grid-template-areas"

gap :: Size LengthUnit -> Size LengthUnit -> Css
gap row col = key "gap" (row, col)

rowGap :: Size LengthUnit -> Css
rowGap = key "row-gap"

columnGap :: Size LengthUnit -> Css
columnGap = key "column-gap"

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
    let
      nested :: [[Text]]
      nested = coerce areas
    in
      nested
      & Prelude.map (Text.intercalate " ")
      & Text.intercalate "\n"
      & value
