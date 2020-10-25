module Generals.Replay.Decode
  ( decode
  )
  where

import Types

import Data.Aeson hiding (decode)
import Data.Aeson.Types
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as L


decode :: FromJSON a => L.ByteString -> a
decode bs =
  case Json.eitherDecode' bs of
    Left e -> error $ e ^. packed
    Right r -> r

instance FromJSON ReplayLocation where
  parseJSON = withObject "ReplayLocation" $
    \v -> ReplayLocation
      <$> v .: "server"
      <*> v .: "id"

instance FromJSON Server where
  parseJSON = withText "Server" parseServer

parseServer :: Text -> Parser Server
parseServer "main" = pure Server_Main
parseServer "bot" = pure Server_Bot
parseServer _ = fail "expected one of 'bot' or 'main' for server"

instance FromJSON Replay where
  parseJSON = withArray "Replay" parseReplay

parseReplay :: Array -> Parser Replay
parseReplay v = Replay
  <$> id
  <*> mapWidth
  <*> mapHeight
  <*> usernames
  <*> cities
  <*> cityArmies
  <*> generals
  <*> mountains
  <*> moves
  <*> afks
  <*> teams
  <*> mapTitle
  <*> swamps
  where
    -- fields
    id = v .@ 1
    mapWidth = v .@ 2
    mapHeight = v .@ 3
    usernames = v .@ 4
    cities = v .@ 6
    cityArmies = v .@ 7
    generals = v .@ 8
    mountains = v .@ 9
    moves = v .@ 10
    afks = v .@ 11
    teams = v .@ 12
    mapTitle = v .@ 13
    swamps = v .@ 16

    -- unused
    version = v .@ 0 :: Parser Int
    stars = v .@ 5 :: Parser Array
    unknown1 = v .@ 14 :: Parser Array
    unknown2 = v .@ 15 :: Parser Array

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

-- plumbing
explicitParseAt :: (Value -> Parser a) -> Array -> Int -> Parser a
explicitParseAt p array key =
  case array ^? ix key of
    Nothing -> fail $ "key " ++ show key ++ " not found"
    Just v  -> p v <?> Index key

(.@) :: FromJSON a => Array -> Int -> Parser a
(.@) = explicitParseAt parseJSON
