module Battleship where

import Html
import Signal exposing (map, foldp,mailbox, Mailbox, Address, Signal)
import BattleshipView exposing (view)
import BattleshipModel exposing (..)

gameMailbox : Mailbox GameModelAction
gameMailbox = mailbox NoOp

state : Signal GameModelAction -> Signal GameModel
state = foldp updateGameModel initModel

main : Signal Html.Html
main =
  Signal.map (view gameMailbox.address) (state gameMailbox.signal)
