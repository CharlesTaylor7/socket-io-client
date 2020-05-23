module Page.Replay.Decode
  ( decode
  )
  where

import Types (Dimensions(..))
import Page.Replay.Types

import Data.Aeson hiding (decode)
import Data.Aeson.Types
import qualified Data.Aeson as Json

decode :: Text -> Replay
decode text =
  case Json.eitherDecode' bs of
    Left e -> error $ e ^. packed
    Right r -> r
  where bs = encodeUtf8 text

instance FromJSON Move where
  parseJSON = withArray "Move" $
    \v -> Move
      <$> v .@ 0
      <*> v .@ 1
      <*> v .@ 2
      <*> (truthy <$> v .@ 3)
      <*> v .@ 4
    where
      truthy :: Int -> Bool
      truthy = (/= 0)


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
    id = v .@ 1
    width = v .@ 2
    height = v .@ 3
    usernames = v .@ 4
    cities = v .@ 6
    cityArmies = v .@ 7
    generals = v .@ 8
    mountains = v .@ 9
    moves = v .@ 10
    afks = v .@ 11
    teams = v .@ 12
    mapTitle = v .@ 13
    -- unused
    version = v .@ 0 :: Parser Int
    stars = v .@ 5 :: Parser Array
    unknown1 = v .@ 14 :: Parser Array
    unknown2 = v .@ 15 :: Parser Array
    unknown3 = v .@ 16 :: Parser Array

-- plumbing
explicitParseAt :: (Value -> Parser a) -> Array -> Int -> Parser a
explicitParseAt p array key =
  case array ^? ix key of
    Nothing -> fail $ "key " ++ show key ++ " not found"
    Just v  -> p v <?> Index key

(.@) :: FromJSON a => Array -> Int -> Parser a
(.@) = explicitParseAt parseJSON
