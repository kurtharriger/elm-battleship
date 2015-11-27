module BattleshipView where

import Html exposing (div,text,img)
import Html.Attributes exposing (style, src)
import Html.Events exposing (on, onWithOptions)
import List exposing (map, map2, concatMap, append, concat, length)
import BattleshipModel exposing (..)
import Signal exposing (Message, mailbox, Address)
import Json.Decode


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

onClick : (() -> Message) -> Html.Attribute
onClick =
  on "click" (Json.Decode.succeed ())

onDragOver : (() -> Message) -> Html.Attribute
onDragOver =
  onWithOptions "dragover" {preventDefault = True, stopPropagation = False} (Json.Decode.succeed ())

onDrop : (() -> Message) -> Html.Attribute
onDrop =
  on "drop" (Json.Decode.succeed ())

onDragStart : (() -> Message) -> Html.Attribute
onDragStart =
  on "dragstart" (Json.Decode.succeed ())


gameSquare : {dispatch: (GridAction -> Message), dropTarget: Bool} -> GridPosition -> Html.Html
gameSquare {dispatch,dropTarget} pos  =
    div
    [
      onClick (always (dispatch (Click pos))),
      onDragOver discard,
      onDrop (always (dispatch (Drop pos))),
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
  styles: List (String, String),
  content: List Html.Html,
  dropTarget: Bool
}

gameGrid : (GridAction -> Message) -> GameGridModel ->  Html.Html
gameGrid dispatch {styles, dropTarget, content}  =
  div
    [
    style (styles `append` [
       ("position", "relative"),
       ("width", (squareSize * 10) |> px),
       ("height", (squareSize * 10) |> px)
     ])
    ]
    (append (map (gameSquare {dispatch = dispatch, dropTarget = dropTarget}) allPositions) content)


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


draggableShip : (PrepareAction -> Message) -> ShipType -> Orientation -> Maybe (ShipType, Orientation)-> Html.Html
draggableShip dispatch shipType orientation selected =
  let highlight =
    case selected of
      Just (selectedShipType, selectedOrientation) ->
        if selectedShipType == shipType && selectedOrientation == orientation then ("border", "1px dashed blue")
        else ("border", "none")
      _ -> ("border", "none")
  in
  div [style [("float","left"), highlight]] [
    shipImg shipType orientation [
      onClick (always (dispatch (SelectShip shipType orientation))),
      onDragStart (always (dispatch (SelectShip shipType orientation)))
    ]
  ]


prepareView : (PrepareAction -> Message) -> PrepareModel -> Html.Html
prepareView dispatch {placed, selected} =
  let
    gridMessage gridAction =
      case (selected, gridAction) of
        (Just (shipType, orientation), Click gridPosition) -> dispatch (PlaceShip shipType orientation gridPosition)
        (Just (shipType, orientation), Drop gridPosition) -> dispatch (PlaceShip shipType orientation gridPosition)
        _ -> discard ()
  in
  div [ style []]
    [
      div [ style [] ] [
        gameGrid gridMessage {
          dropTarget = True,
          styles = [("margin", "50px"),("float", "left")],
          content = (map ship placed)
        }
      ],
      div [ style [("margin", "50px"), ("float", "left")] ] [
        text ("Click grid to place the " ++ (shipName <| nextShipToPlace placed)),
        div [style [("height", "100px")]] [
          draggableShip dispatch AircraftCarrier Horizontal selected,
          draggableShip dispatch Battleship Horizontal selected,
          draggableShip dispatch Cruiser Horizontal selected,
          draggableShip dispatch Submarine Horizontal selected,
          draggableShip dispatch Patrol Horizontal selected
        ],
        div [] [
          draggableShip dispatch AircraftCarrier Vertical selected,
          draggableShip dispatch Battleship Vertical selected,
          draggableShip dispatch Cruiser Vertical selected,
          draggableShip dispatch Submarine Vertical selected,
          draggableShip dispatch Patrol Vertical selected
        ]
      ]
    ]

playView : (PlayAction -> Message) -> PlayModel -> Html.Html
playView dispatch {setup, missileLog} =
  let clickHandler action =
        case action of
          Click pos -> (dispatch (Fire pos))
          _ -> (discard ())
  in
  div [ ]
  [
    gameGrid discard {
      dropTarget = False,
      styles = [("margin", "50px"),("float", "left")],
      content = (map ship setup)
    },
    gameGrid clickHandler {
      dropTarget = False,
      styles = [("margin", "50px"),("float", "left")],
      content = (map missileIndicator missileLog)
    }
  ]

view : (GameModelAction -> Message) -> GameModel -> Html.Html
view dispatch model =
  case model of
    Preparing prepareModel ->
      prepareView (dispatch << Prepare) prepareModel

    Playing playModel ->
      playView (dispatch << Play) playModel


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
main = view discard (Preparing { initPreparingModel | placed = [(AircraftCarrier, (1,1), Horizontal)]})
