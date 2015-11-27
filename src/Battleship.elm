module Battleship where

import Html
import Signal exposing (map, foldp,mailbox, Mailbox, Address, Signal, message, Message)
import BattleshipView exposing (view)
import BattleshipModel exposing (..)

gameMailbox : Mailbox GameModelAction
gameMailbox = mailbox NoOp

gameDispatcher : (GameModelAction -> Message)
gameDispatcher = message gameMailbox.address

debugGameDispatcher m =
  gameDispatcher (Debug.log "message" m)

state : Signal GameModelAction -> Signal GameModel
state = foldp updateGameModel initModel

main : Signal Html.Html
main =
  Signal.map (view debugGameDispatcher) (state gameMailbox.signal)
