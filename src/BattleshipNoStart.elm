module Battleship where

import Html
import Signal exposing (Mailbox, Address, Signal, message, mailbox)
import BattleshipView
import BattleshipModel exposing (..)
import StartApp.Simple as StartApp


view : Address GameModelAction -> GameModel -> Html.Html
view address =
  BattleshipView.view (message address)


maybeView : Address GameModelAction -> Maybe GameModel -> Html.Html
maybeView address model =
  case Debug.log "model" model of
    Just model -> view address model
    Nothing -> Html.text "Model did not initialize. Check console for log output."

update : GameModelAction -> Maybe GameModel -> Maybe GameModel
update action model =
  case Debug.log "update" (model, action) of
    (Nothing, Initialize seed) -> Just <| initModel seed
    (Nothing, _) -> Nothing
    (Just model, _) -> Just <| BattleshipModel.updateGameModel action model

app : StartApp.Config (Maybe GameModel) GameModelAction
app = {
    model = Nothing,
    view = maybeView,
    update = update
  }

main : Signal Html.Html
main =
  let gameMailbox = mailbox NoOp
  in
  Signal.map
    (app.view gameMailbox.address)
    (Signal.foldp app.update app.model
      (Signal.map Initialize initialSeed))
