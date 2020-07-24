{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
module Clay.Grid where

import Clay
import Clay.Stylesheet

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Coerce (coerce)
import GHC.Exts (IsList(..))


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
    value $
    Text.intercalate "\n" $
    fmap (Text.intercalate " ") $
    (coerce areas :: [[Text]])

