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


shipImageSrc : ShipType -> Orientation -> String
shipImageSrc shipType orientation =
  case (shipType, orientation) of
    (AircraftCarrier, Horizontal) -> "img/AircraftCarrier.png"
    (Battleship, Horizontal) -> "img/Battleship.png"
    (Cruiser, Horizontal) -> "img/Cruiser.png"
    (Submarine, Horizontal) -> "img/Submarine.png"
    (Patrol, Horizontal) -> "img/PatrolBoat.png"
    (AircraftCarrier, Vertical) -> "img/AircraftCarrierVertical.png"
    (Battleship, Vertical) -> "img/BattleshipVertical.png"
    (Cruiser, Vertical) -> "img/CruiserVertical.png"
    (Submarine, Vertical) -> "img/SubmarineVertical.png"
    (Patrol, Vertical) -> "img/PatrolBoatVertical.png"

shipImg : ShipType -> Orientation -> List Html.Attribute -> Html.Html
shipImg  shipType orientation attributes =
  img (attributes `append` [src <| shipImageSrc shipType orientation]) []

type alias Style = (String, String)

gridPositioned : GridPosition -> List Style
gridPositioned (x,y) =
  [
    ("position", "absolute"),
    ("top", offset x |> px),
    ("left", offset y |> px)
  ]

ship : (ShipType, GridPosition, Orientation) -> Html.Html
ship (shipType, position, orientation) =
  shipImg shipType orientation [style <| gridPositioned position]


allPositions : List GridPosition
allPositions =
  [0..9]
  |> concatMap (\i -> [0..9]
  |> map (\j -> (i,j)))


type GridAction
  = Click GridPosition
  | Drop GridPosition

gameSquare : {address: Address GridAction, dropTarget: Bool} -> GridPosition -> Html.Html
gameSquare {address,dropTarget} pos  =
    div
    [
      onClick address (Click pos),
      style
        <| gridPositioned pos
          `append`
          [
            ("width", squareSize |> px),
            ("height", squareSize |> px),
            ("border","1px solid black"),
            ("background-color", "blue")
          ]
    ] []


type alias GameGridModel = {
  address: Address GridAction,
  styles: List (String, String),
  content: List Html.Html,
  dropTarget: Bool
}

gameGrid : GameGridModel ->  Html.Html
gameGrid {address, styles, dropTarget, content}  =
  div
    [
    style (styles `append` [
       ("position", "relative"),
       ("width", (squareSize * 10) |> px),
       ("height", (squareSize * 10) |> px)
     ])
    ]
    (append (map (gameSquare {address = address, dropTarget = dropTarget}) allPositions) content)


missileIndicator : MissileResult -> Html.Html
missileIndicator result =
  let (color, pos) = case result of
    Miss pos -> ("white", pos)
    Hit pos -> ("red", pos)
  in
  div [
    style ((gridPositioned pos) `append` [
      ("width", squareSize |> px),
      ("height", squareSize |> px)
    ])
  ]
  [
    div [
      style [
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


draggableShip : Address PrepareAction -> ShipType -> Orientation -> Maybe (ShipType, Orientation)-> Html.Html
draggableShip address shipType orientation selected =
  let highlight =
    case selected of
      Just (selectedShipType, selectedOrientation) ->
        if selectedShipType == shipType && selectedOrientation == orientation then ("border", "1px dashed blue")
        else ("border", "none")
      _ -> ("border", "none")
  in
  div [style [("float","left"), highlight]] [
    shipImg shipType orientation [
      onClick (Signal.forwardTo address identity) (SelectShip shipType orientation)
    ]
  ]



prepareView :  Address PrepareAction -> PrepareModel -> Html.Html
prepareView address {placed, selected} =
  let clickHandler action =
      let gridPosition =
        case action of
          Click gridPosition -> gridPosition
          Drop gridPosition -> gridPosition
      in
        case selected of
          Just (selected, orientation) -> PlaceShip selected orientation gridPosition
          Nothing -> PrepareNoOp
  in
  div [ style []]
    [
      div [ style [] ] [
        gameGrid {
          address = (Signal.forwardTo address clickHandler),
          dropTarget = True,
          styles = [("margin", "50px"),("float", "left")],
          content = (map ship placed)
        }
      ],
      div [ style [("margin", "50px"), ("float", "left")] ] [
        text ("Click grid to place the " ++ (shipName <| nextShipToPlace placed)),
        div [style [("height", "100px")]] [
          draggableShip address AircraftCarrier Horizontal selected,
          draggableShip address Battleship Horizontal selected,
          draggableShip address Cruiser Horizontal selected,
          draggableShip address Submarine Horizontal selected,
          draggableShip address Patrol Horizontal selected
        ],
        div [] [
          draggableShip address AircraftCarrier Vertical selected,
          draggableShip address Battleship Vertical selected,
          draggableShip address Cruiser Vertical selected,
          draggableShip address Submarine Vertical selected,
          draggableShip address Patrol Vertical selected
        ]
      ]
    ]

playView : (Address (Maybe PlayAction)) -> PlayModel -> Html.Html
playView address {setup, missileLog} =
  let nowhere = (mailbox ()).address
      clickHandler action =
        case action of
          Click pos -> Just (Fire pos)
          _ -> Nothing
  in
  div [ ]
  [
    gameGrid {
      address = (Signal.forwardTo nowhere (always ())),
      dropTarget = False,
      styles = [("margin", "50px"),("float", "left")],
      content = (map ship setup)
    },
    gameGrid {
      address = (Signal.forwardTo address clickHandler),
      dropTarget = False,
      styles = [("margin", "50px"),("float", "left")],
      content = (map missileIndicator missileLog)
    }
  ]

view : (Address GameModelAction) -> GameModel -> Html.Html
view address model =
  case model of
    Preparing prepareModel ->
      prepareView (Signal.forwardTo address Prepare) prepareModel

    Playing playModel ->
      playView (Signal.forwardTo address Play) playModel


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
-- main = view (mailbox NoOp).address (Playing (ships, missileLog))
main = view (mailbox NoOp).address (Preparing { initPreparingModel | placed = [(AircraftCarrier, (1,1), Horizontal)]})
