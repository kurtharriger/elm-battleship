module Main where

import Html exposing (div,text,img)
import Html.Attributes exposing (style, src)
import List exposing (map, map2, concatMap, append, concat)
import BattleshipModel exposing (..)


squareSize : Int
squareSize = 32


offset : Int -> Int
offset x = x*squareSize


px : Int -> String
px x = (toString x) ++ "px"


transform : Orientation -> Html.Attribute
transform orientation =
  case orientation of
    Horizontal -> style []
    Vertical -> style [("transform", "rotate(90deg)")]


shipImage : ShipType -> String
shipImage shipType =
  case shipType of
    AircraftCarrier -> "img/AircraftCarrier.png"
    Battleship -> "img/Battleship.png"
    Cruiser -> "img/Cruiser.png"
    Submarine -> "img/Submarine.png"
    Patrol -> "img/PatrolBoat.png"


shipEndPos : ShipType -> GridPosition -> Orientation -> GridPosition
shipEndPos shipType position orientation =
  let (x,y) = position
  in
  case orientation of
    Horizontal -> (x + shipLength shipType, y + 1)
    Vertical -> (x + 1, y + shipLength shipType )


ship : (ShipType, GridPosition, Orientation) -> Html.Html
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


allPositions : List GridPosition
allPositions =
  [0..10]
  |> concatMap (\i -> [0..10]
  |> map (\j -> (i,j)))


gameSquare : GridPosition -> Html.Html
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


gameGrid : List Html.Html -> Html.Html
gameGrid children =
  div
    [
     style
     [
       ("position", "relative"),
       ("margin", "50px")
     ]
    ]
    (append (map gameSquare allPositions) children)


--
-- Test Rendering
--

ships : List ShipPlacement
ships = [
   (AircraftCarrier, (1,1), Vertical),
   (Battleship, (2,2), Horizontal),
   (Cruiser, (5,5), Vertical),
   (Submarine, (7,8), Vertical),
   (Patrol, (9,6), Horizontal)
  ]


main : Html.Html
main = gameGrid <| map ship ships
