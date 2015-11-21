module Main where

import Color exposing (blue, white, rgb, black)
import Html exposing (div,text)
import Html.Attributes exposing (style)
import Graphics.Element exposing (show,layers)
import List exposing (map, map2, concatMap, append, concat)


type alias Pos = (Int,Int)

type LogEntry = Hit Pos
              | Miss Pos

type alias MissleLog = List LogEntry

type ShipType = AircraftCarrier
              | Battleship
              | Submarine
              | Cruiser
              | Patrol

shipLength : ShipType -> Int
shipLength shipType =
  case shipType of
      AircraftCarrier -> 5
      Battleship -> 4
      Submarine -> 3
      Cruiser -> 3
      Patrol -> 2

type Orientation = Horizontal
                 | Vertical

type alias ShipPlacement = ShipType Pos Orientation

-- gameSquare : Pos -> List Element
gameSquare (i,j) =
  div
  [ style [
      ("width", "40px"),
      ("height", "40px"),
      ("border","1px solid black"),
      ("position", "absolute"),
      ("top", (toString (i*40)) ++ "px"),
      ("left", (toString (j*40)) ++ "px"),
      ("background-color", "blue")
    ]
  ] []

gameBoard =
   indicies
   |> map gameSquare
   |> (\c -> append c [ship (2,2)] )
   |> div [ style
    [
      ("position", "relative"),
      ("margin", "50px")
    ]]

ship (i,j) =
  div
  [ style [
      ("width", "100px"),
      ("height", "20px"),
      ("position", "absolute"),
      ("top", (i*40 + 20 |> toString) ++ "px"),
      ("left", (j*40 + 10 |> toString) ++ "px"),
      ("background-color", "black")
    ]
  ] []

--
indicies : List Pos
indicies =
  [0..10]
  |> concatMap (\i -> [0..10]
  |> map (\j -> (i,j)))

main = gameBoard
