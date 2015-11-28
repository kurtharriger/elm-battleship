module Battleship where

import Html
import Signal exposing (Address, Signal, Message, message)
import BattleshipView
import BattleshipModel exposing (..)
import StartApp exposing (start)
import Task
import Effects exposing (Effects, Never)
import Mouse

view : Address GameModelAction -> GameModel -> Html.Html
view address =
  BattleshipView.view (message address)


maybeView : Address GameModelAction -> Maybe GameModel -> Html.Html
maybeView address model =
  case Debug.log "model" model of
    Just model -> view address model
    Nothing -> Html.text "Model did not initialize. Check console for log output."

update : GameModelAction -> Maybe GameModel -> ( Maybe GameModel, Effects GameModelAction )
update action model =
  case Debug.log "update" (model, action) of
    (Nothing, Initialize seed) -> (Just <| initModel seed, Effects.none)
    (Nothing, _) -> (Nothing, Effects.none)
    (Just model, _) -> (Just <| BattleshipModel.updateGameModel action model, Effects.none)

app : StartApp.App (Maybe GameModel)
app =
  StartApp.start
    { init = (Nothing, Effects.none)
    , update = update
    , view = maybeView
    , inputs = [Signal.map (Debug.log "init" << Initialize) initialSeed,
                Signal.map (always NoOp) Mouse.position]
    }

main : Signal Html.Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
