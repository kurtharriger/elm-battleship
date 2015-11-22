module BattleshipView where

import Html exposing (div,text,img)
import Html.Attributes exposing (style, src)
import Html.Events exposing (onClick)
import List exposing (map, map2, concatMap, append, concat, length)
import BattleshipModel exposing (..)
import Signal exposing (Message, mailbox, Address)


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
      img [src (shipImage shipType)] []
    ]
  ]


allPositions : List GridPosition
allPositions =
  [0..10]
  |> concatMap (\i -> [0..10]
  |> map (\j -> (i,j)))


positionStyles : GridPosition -> List (String, String)
positionStyles (i,j)  =
  [
    ("position", "absolute"),
    ("top", offset i |> px),
    ("left", offset j |> px),
    ("width", squareSize |> px),
    ("height", squareSize |> px)
  ]

gameSquare : (Address GridPosition) -> GridPosition -> Html.Html
gameSquare clickAddress pos  =
    div
    [
      onClick clickAddress pos,
      style <|
        append (positionStyles pos)
        [
          ("border","1px solid black"),
          ("background-color", "blue")
        ]
    ] []


gameGrid : (Address GridPosition) -> List Html.Html ->  Html.Html
gameGrid clickAddress children  =
  div
    [
     style
     [
       ("position", "relative"),
       ("width", (offset 11) |> px),
       ("height", (offset 11) |> px)
     ]
    ]
    (append (map (gameSquare clickAddress) allPositions) children)


--
-- Test Rendering
--
missileIndicator : MissileResult -> Html.Html
missileIndicator result =
  let (color, pos) = case result of
    Miss pos -> ("white", pos)
    Hit pos -> ("red", pos)
  in
  div
    [
      style <| positionStyles pos
    ]
    [
      div
      [
        style <|

          [
            ("width", 20 |> px),
            ("height", 20 |> px),
            ("position", "relative"),
            ("left", "50%"),
            ("top", "50%"),
            ("margin", "-10px 0 0 -10px"),
            ("background-color", color),
            ("border-radius", "10px")
          ]
      ] []
    ]

prepareView : Address PrepareModelAction -> PrepareModel -> Html.Html
prepareView address {placed, selected, orientation} =
  div [ style []]
    [
      div [ style [("margin", "50px"),("float", "left")]]
        [gameGrid
          (Signal.forwardTo address (\pos -> PlaceShip selected orientation pos))
          (map ship placed)],
      div [ style [("margin", "50px"),("float", "left")]] [
        text ("Click grid to place the " ++ (shipName <| nextShipToPlace placed))
      ]
    ]

view : (Address GameModelAction) -> GameModel -> Html.Html
view address model =
  case model of
    Preparing prepareModel ->
      prepareView
        (Signal.forwardTo address (\action -> PrepareAction (updatePreparing action prepareModel)))
        prepareModel

    Playing (ships, log) ->
      div [ ]
        [
          div [ style [("margin", "50px"), ("float", "left")]] [gameGrid (Signal.forwardTo address (always NoOp)) <| map ship ships ],
          div [ style [("margin", "50px"),("float", "left")]] [gameGrid (Signal.forwardTo address (always NoOp)) <|map missileIndicator log]
        ]


nextShipToPlace : List ShipPlacement -> ShipType
nextShipToPlace ships =
  case (length ships) of
    0 -> AircraftCarrier
    1 -> Battleship
    2 -> Cruiser
    3 -> Submarine
    _ -> Patrol



--
-- Example state for testing
--

ships : List ShipPlacement
ships = [
   (AircraftCarrier, (1,1), Vertical),
   (Battleship, (2,2), Horizontal),
   (Cruiser, (5,5), Vertical),
   (Submarine, (7,8), Vertical),
   (Patrol, (9,6), Horizontal)
  ]

missileLog : List MissileResult
missileLog =
  [
    Hit (1,2),
    Miss (3, 4),
    Miss (8, 2)
  ]


main : Html.Html
main = view (mailbox NoOp).address (Playing (ships, missileLog))
-- main = view (mailbox NoOp).address (Preparing { initPreparingModel | placed = [(AircraftCarrier, (1,1), Horizontal)]})
