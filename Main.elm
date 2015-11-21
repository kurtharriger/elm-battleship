module Main where

import Color exposing (blue, white, rgb, black)
import Html exposing (div,text,img)
import Html.Attributes exposing (style, src)
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

shipEndPos shipType position orientation =
  let (x,y) = position
  in
  case orientation of
    Horizontal -> (x + shipLength shipType, y + 1)
    Vertical -> (x + 1, y + shipLength shipType )

squareSize = 32
offset x = x*squareSize
px x = (toString x) ++ "px"

transform orientation =
  case orientation of
    Horizontal -> style []
    Vertical -> style [("transform", "rotate(90deg)")]


shipImage shipType =
  case shipType of
    AircraftCarrier -> "img/AircraftCarrier.png"
    Battleship -> "img/Battleship.png"
    Cruiser -> "img/Cruiser.png"
    Submarine -> "img/Submarine.png"
    Patrol -> "img/PatrolBoat.png"


ship (shipType, position, orientation) =
  let (x,y) = position
      (x2,y2) = shipEndPos shipType position orientation
  in
  div [
    style [
      ("width", (offset x2) - (offset x) |> px),
      ("height", (offset y2) - (offset y) |> px),
      ("position", "absolute"),
      ("top", offset x |> px),
      ("left", offset y |> px)
    ]
  ] [
    div [transform orientation]
    [
      img [src (shipImage shipType) ] []
    ]
  ]
--
indicies =
  [0..10]
  |> concatMap (\i -> [0..10]
  |> map (\j -> (i,j)))


-- gameSquare : Pos -> List Element
gameSquare (i,j) =
  div
  [ style [
      ("width", squareSize |> px),
      ("height", squareSize |> px),
      ("border","1px solid black"),
      ("position", "absolute"),
      ("top", offset i |> px),
      ("left", offset j |> px),
      ("background-color", "blue")
    ]
  ] []

gameBoard =
   indicies
   |> map gameSquare
   |> (\c -> append c (map ship ships) )
   |> div
      [
        style
        [
          ("position", "relative"),
          ("margin", "50px")
        ]
      ]

ships : List (ShipType, Pos, Orientation)
ships = [
   (AircraftCarrier, (1,1), Vertical),
   (Battleship, (2,2), Horizontal),
   (Cruiser, (5,5), Vertical),
   (Submarine, (7,8), Vertical),
   (Patrol, (9,6), Horizontal)
  ]

main = gameBoard
