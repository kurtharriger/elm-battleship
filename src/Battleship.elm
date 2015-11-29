module Battleship where

import Html
import Signal exposing (Mailbox, Address, Signal, message, mailbox, foldp)
import Signal.Extra exposing (foldp')
import BattleshipView
import BattleshipModel exposing (..)
import Task exposing (Task)



view : Address GameModelAction -> GameModel -> Html.Html
view address =
  BattleshipView.view (message address)

gameMailbox : Mailbox GameModelAction
gameMailbox = mailbox NoOp

main : Signal Html.Html
main =
  Signal.map
    (view gameMailbox.address)
    (foldp' (snd >> updateGameModel)
      (\(seed, action) -> Debug.log "initilize" (initModel seed))
      (Signal.map2 (,) initialSeed gameMailbox.signal))


send : GameModelAction -> Task GameModelAction ()
send = (Signal.send gameMailbox.address)

port tasks : Task GameModelAction ()
port tasks =
  send (PlayGame BattleshipView.ships) 
