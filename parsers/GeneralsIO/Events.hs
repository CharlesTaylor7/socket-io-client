{-# Language DeriveGeneric #-}
module GeneralsIO.Events
  ( SetUsernameError(..)
  , SetUsernameResponse(..)
  )
  where

import GHC.Generics (Generic)
import qualified Data.Aeson as Json


data SetUsernameResponse
  = UsernameValid
  | UsernameTaken
  | UsernameTooLong
  | UsernameMustBeginWithBot
  | UsernameCannotBeChanged
  | UsernameProfanity
  deriving (Generic)


-- | errors messages mapped to union
-- empty strings means the username was successfully chosen
setUserNameResponses :: [(String, SetUsernameResponse)]
setUserNameResponses =
  [ ( ""
    , UsernameValid
    )
  , ( "You already have a username! Only Supporters can change usernames."
    , UsernameCannotBeChanged
    )
  , ( "Usernames of Bots must begin with [Bot]"
    , UsernameMustBeginWithBot
    )
  , ( "Username too long."
    , UsernameTooLong
    )
  , ( "This username is already taken."
    , UsernameTaken
    )
  , ( "This username contains profanity."
    , UsernameProfanity
    )
  ]
