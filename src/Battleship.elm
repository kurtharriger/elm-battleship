module Battleship where

import Html
import Signal exposing (Mailbox, Address, Signal, message, mailbox, foldp)
import Signal.Extra exposing (foldp')
import BattleshipView
import BattleshipModel exposing (..)

view : Address GameModelAction -> GameModel -> Html.Html
view address =
  BattleshipView.view (message address)

main : Signal Html.Html
main =
  let gameMailbox = mailbox NoOp
  in
  Signal.map
    (view gameMailbox.address)
    (foldp' (snd >> updateGameModel)
      (\(seed, action) -> Debug.log "initilize" (initModel seed))
      (Signal.map2 (,) initialSeed gameMailbox.signal))
