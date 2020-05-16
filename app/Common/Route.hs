{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

import Control.Category
import qualified Data.Text as T

import Obelisk.Route
import Obelisk.Route.TH

import Common.Imports hiding ((.))
import Common.Types

fullRouteEncoder :: AppRouteEncoder
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
  )
  (\case
      FrontendRoute_Setup -> PathEnd $ unitEncoder mempty
      FrontendRoute_NewGame -> PathSegment "new-game" $ newGameEncoder
      FrontendRoute_Game -> PathSegment "game" $ gameIdEncoder
  )

-- business encoders
type PathSegmentEncoder = Encoder ErrorMonad ErrorMonad

newGameEncoder :: PathSegmentEncoder GameConfig PageName
newGameEncoder =
  endPathEncoder .
  mapEncoder "bots" .
  prismEncoder (commaSeparated . coerced)

gameIdEncoder :: PathSegmentEncoder Id PageName
gameIdEncoder =
  endPathEncoder .
  mapEncoder "id" .
  prismEncoder (from packed . _Show . from _Id)

-- utility isos
delimited :: Text -> Iso' Text [Text]
delimited separator = iso to from
  where
    to = T.splitOn separator
    from = T.intercalate separator

commaSeparated :: Iso' Text [Text]
commaSeparated = delimited ","

-- utility encoders

mkEncoder
  :: Applicative check
  => (encoded -> parse decoded)
  -> (decoded -> encoded)
  -> Encoder check parse decoded encoded
mkEncoder = (unsafeMkEncoder .) . EncoderImpl

constFirstEncoder :: (Show a, Eq a) => a -> PathSegmentEncoder b (a, b)
constFirstEncoder constant = mkEncoder decode encode
  where
    decode (x, s)
      | x == constant = Right s
      | otherwise     = Left $ "expected tuple first argument to be: " <> show constant <> "but was: " <> show x
    encode s = (constant, s)

endPathEncoder :: PathSegmentEncoder QueryParams PageName
endPathEncoder = constFirstEncoder []

mapEncoder :: (Show key, Ord key) => key -> PathSegmentEncoder value (Map key (Maybe value))
mapEncoder k = mkEncoder decode encode
  where
    encode v = mempty & at k ?~ Just v
    decode map =
      case join $ map ^. at k of
        Just x -> Right x
        Nothing -> Left $ "mapEncoder: expected entry for key: " <> show k
