module Battleship where

import Html
import Signal exposing (Mailbox, Address, Signal, message, mailbox, foldp)
import Signal.Extra exposing (foldp')
import BattleshipView
import BattleshipModel exposing (..)
import Task exposing (Task)
import Random


view : Address GameModelAction -> GameModel -> Html.Html
view address =
  BattleshipView.view (message address)

gameMailbox : Mailbox GameModelAction
gameMailbox = mailbox NoOp


watches : GameModel -> GameModel
watches model =
  let _ = (Debug.watch "want to cheat?" model.opposingSetup)
      _  = Debug.watch "hits" <| List.length <| List.filter (.result >> isHit) model.missileLog
  in
  model


main : Signal Html.Html
main =
  Signal.map
    (view gameMailbox.address)
    (Signal.map watches
      (foldp' (snd >> Debug.log "message" >> updateGameModel)
        (\(seed, action) -> initModel seed)
        (Signal.map2 (,) initialSeed gameMailbox.signal)))

--
-- send : GameModelAction -> Task GameModelAction ()
-- send = (Signal.send gameMailbox.address)
--
-- port tasks : Task GameModelAction ()
-- port tasks =
--   send (PlayGame BattleshipView.ships)
