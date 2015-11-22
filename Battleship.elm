module Battleship where

import Html
import BattleshipView exposing (view)
import BattleshipModel exposing (..)


main : Html.Html
main = view (Preparing [])
