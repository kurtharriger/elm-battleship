module Battleship where

import Html
import Signal exposing (Address, Signal, message)
import BattleshipView
import BattleshipModel exposing (..)
import StartApp.Simple exposing (start)
import Random


view : Address GameModelAction -> GameModel -> Html.Html
view address =
  BattleshipView.view (message address)


main : Signal Html.Html
main =
  start {
    model = initModel (Random.initialSeed 42),
    view = view,
    update = updateGameModel
  }
