module UI.Events
  ( handleEvent
  )
  where

import Types
import UI.Types

import Brick hiding (Widget, Horizontal, Vertical, Both)
import qualified Brick.Forms as Brick

import qualified Graphics.Vty as V


scrollAmount :: Int
scrollAmount = 3

handleEvent :: BrickEvent Name CustomEvent -> AppState -> EventM Name (Next AppState)
handleEvent e@(VtyEvent (V.EvKey key [])) =
  case key of
    V.KEsc -> halt

    -- scroll keys
    V.KChar 'h' -> \s -> do
      hScrollBy (viewportScroll GridView) (-scrollAmount)
      continue s

    V.KChar 'l' -> \s -> do
      hScrollBy (viewportScroll GridView) (scrollAmount)
      continue s

    V.KChar 'j' -> \s -> do
      vScrollBy (viewportScroll GridView) (scrollAmount)
      continue s

    V.KChar 'k' -> \s -> do
      vScrollBy (viewportScroll GridView) (-scrollAmount)
      continue s

    -- advance replay
    V.KRight -> \s -> do
      invalidateCacheEntry GridView
      continue $
        s & #turnIndex . #_TurnIndex +~ 1

    -- rewind replay
    V.KLeft -> \s -> do
      invalidateCacheEntry GridView
      continue $
        s & #turnIndex . #_TurnIndex -~ 1
    _ -> handleFormEvents e
handleEvent e = handleFormEvents e

handleFormEvents = handleFormInput #jumpToTurnForm validateJumpToTurn propagate

propagate :: Brick.Form TurnIndex event Name -> AppState -> EventM Name AppState
propagate form appState = do
  let turn = Brick.formState form

  invalidateCacheEntry GridView
  pure $ appState & #turnIndex .~ turn


validateJumpToTurn :: AppState -> Brick.Form TurnIndex event name -> Brick.Form TurnIndex event name
validateJumpToTurn appState form = do
  let
    lower = 0
    upper = appState ^. #history . to length . to (subtract 1)
    turn = Brick.formState form

  case turn of
    TurnIndex n
      | n < lower -> Brick.updateFormState (TurnIndex lower) form
      | n > upper -> Brick.updateFormState (TurnIndex upper) form
      | otherwise -> form


handleFormInput
  :: Eq name
  => Lens' appState (Brick.Form formState event name)
  -- ^ lens from the app state into the brick form
  -> (appState -> Brick.Form formState event name -> Brick.Form formState event name)
  -- ^ custom validation on the form
  -> (Brick.Form formState event name -> appState -> EventM name appState)
  -- ^ propagate changes on the form back up to the app state
  -> BrickEvent name event
  -> appState
  -> EventM name (Next appState)
handleFormInput lens validate propagate event appState = do
  brickForm <- Brick.handleFormEvent event $ appState ^. lens
  brickForm <- pure $ validate appState brickForm
  appState <- propagate brickForm appState
  continue $
    appState
      & lens .~ brickForm
