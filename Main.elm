module Main where

import Color exposing (blue, white, rgb, black)
import Graphics.Collage exposing (Form, collage, filled, rect, outlined, solid, group, toForm, move)
import Graphics.Element exposing (show,layers)
import List exposing (map, map2, concatMap, append)


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
  let r = rect 20 20
  in
  group [
    r |> filled blue,
    r |> outlined (solid black)
  ]
  |> move ((toFloat i*20),(toFloat j*20))


gameBoard =
   indicies
   |> map gameSquare
   |> group
   |> move (-100,-100)
   |> (\f -> collage 300 300 [f])

--
indicies : List Pos
indicies =
  [0..10]
  |> concatMap (\i -> [0..10]
  |> map (\j -> (i,j)))

main = gameBoard
