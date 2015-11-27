module Battleship where

import Html
import Signal exposing (map, foldp,mailbox, Mailbox, Address, Signal, message, Message)
import BattleshipView exposing (view)
import BattleshipModel exposing (..)

gameMailbox : Mailbox GameModelAction
gameMailbox = mailbox NoOp

gameDispatcher : (GameModelAction -> Message)
gameDispatcher = message gameMailbox.address

state : Signal GameModelAction -> Signal GameModel
state = Debug.watch "state" <| foldp updateGameModel initModel

logging : Signal a -> Signal a
logging =
  Signal.map (Debug.log "signal")

main : Signal Html.Html
main =
  Signal.map (view gameDispatcher) ((logging >> state) gameMailbox.signal)
