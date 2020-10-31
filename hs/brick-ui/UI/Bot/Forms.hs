module UI.Bot.Forms where

import Generals.Types
import Brick.Forms (Form, newForm, editShowableField, setFormFocus)

import UI.Bot.Types


newJumpToTurnForm :: TurnIndex -> Form TurnIndex e Name
newJumpToTurnForm =
  setFormFocus JumpToTurnInput .
  newForm
    [ editShowableField _TurnIndex JumpToTurnInput
    ]
