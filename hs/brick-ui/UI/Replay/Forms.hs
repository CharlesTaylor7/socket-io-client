module UI.Forms where

import Types
import Brick.Forms (Form, newForm, editShowableField, setFormFocus)

import UI.Types


newJumpToTurnForm :: TurnIndex -> Form TurnIndex e Name
newJumpToTurnForm =
  setFormFocus JumpToTurnInput .
  newForm
    [ editShowableField _TurnIndex JumpToTurnInput
    ]
