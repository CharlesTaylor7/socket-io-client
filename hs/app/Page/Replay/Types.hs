-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE ExplicitForAll #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE RecursiveDo #-}
-- {-# LANGUAGE DeriveGeneric #-}
module Page.Replay.Types
  ( Replay(..)
  , decode
  )
  where

import Generals.Imports hiding (Value)
import Generals.Types (Dimensions(..))

import Data.Aeson hiding ((.:), decode)
import qualified Data.Aeson as Json
import Data.Aeson.Types hiding ((.:))

decode :: Text -> Replay
decode text =
  case Json.eitherDecode' bs of
    Left e -> error $ e ^. packed
    Right r -> r
  where bs = encodeUtf8 text

data Replay = Replay
  { id :: Text
  , dimensions :: Dimensions
  , usernames :: Array
  -- , stars :: Array
  , cities :: Array
  , cityArmies :: Array
  , generals :: Array
  , mountains :: Array
  , moves :: Array
  , afks :: Array
  , teams :: Maybe Array
  , mapTitle :: Maybe Text
  -- , unknown1 :: Array
  -- , unknown2 :: Array
  -- , unknown3 :: Array
  }
  deriving (Eq, Show)



instance FromJSON Replay where
  parseJSON = withArray "Replay" parseReplay

parseReplay :: Array -> Parser Replay
parseReplay v = Replay
  <$> id
  <*> dimensions
  <*> usernames
  <*> cities
  <*> cityArmies
  <*> generals
  <*> mountains
  <*> moves
  <*> afks
  <*> teams
  <*> mapTitle
  where
    dimensions = Dimensions <$> width <*> height
    -- fields
    id = v .: 1
    width = v .: 2
    height = v .: 3
    usernames = v .: 4
    cities = v .: 6
    cityArmies = v .: 7
    generals = v .: 8
    mountains = v .: 9
    moves = v .: 10
    afks = v .: 11
    teams = v .: 12
    mapTitle = v .: 13
    -- unused
    version = v .: 0 :: Parser Int
    stars = v .: 5 :: Parser Array
    unknown1 = v .: 14 :: Parser Array
    unknown2 = v .: 15 :: Parser Array
    unknown3 = v .: 16 :: Parser Array

-- plumbing
explicitParseAt :: (Value -> Parser a) -> Array -> Int -> Parser a
explicitParseAt p array key =
  case array ^? ix key of
    Nothing -> fail $ "key " ++ show key ++ " not found"
    Just v  -> p v <?> Index key

(.:) :: FromJSON a => Array -> Int -> Parser a
(.:) = explicitParseAt parseJSON
