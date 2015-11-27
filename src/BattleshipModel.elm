module BattleshipModel where

import List exposing ((::), length, map, any)
import Signal exposing (Mailbox, Message, mailbox)

type ShipType
  = AircraftCarrier
  | Battleship
  | Submarine
  | Cruiser
  | Patrol


type Orientation
  = Horizontal
  | Vertical


type alias GridPosition
  = (Int, Int)


type alias ShipPlacement
  = (ShipType, GridPosition, Orientation)


type MissileResult
  = Hit GridPosition
  | Miss GridPosition


type alias MissileLog = List MissileResult


type alias PrepareModel = {
    placed: List ShipPlacement,
    selected: Maybe (ShipType, Orientation)
  }

type alias PlayModel = {
  setup: List ShipPlacement,
  missileLog: MissileLog
}

type GameModel
  = Preparing PrepareModel
  | Playing PlayModel


type PrepareAction
  = PlaceShip ShipPlacement
  | SelectShip ShipType Orientation

type PlayAction
  = Fire GridPosition

type GameModelAction
  = Prepare PrepareAction
  | PlayGame (List ShipPlacement)
  | Play PlayAction
  | NoOp


initPreparingModel : PrepareModel
initPreparingModel  =
  {
    placed = [],
    selected = Nothing
  }


nextShipToPlace : List ShipPlacement -> ShipType
nextShipToPlace ships =
  case (length ships) of
    0 -> AircraftCarrier
    1 -> Battleship
    2 -> Cruiser
    3 -> Submarine
    _ -> Patrol


getShipPositions : ShipPlacement -> Maybe (List GridPosition)
getShipPositions (shipType, (x,y), orientation) =
  case orientation of
      Vertical ->
        let end = x + (shipLength shipType) - 1
        in
        if end >= 10 then Nothing
        else Just <| map (\i -> (i,y)) [x..end]
      Horizontal ->
        let end = y + (shipLength shipType) - 1
        in
        if end >= 10 then Nothing
        else Just <| map (\i -> (x,i)) [y..end]

hitShip : ShipPlacement -> GridPosition -> Bool
hitShip placement pos =
  case getShipPositions placement of
    Just positions -> any ((==) pos) positions
    _ -> False

canPlaceShip : List ShipPlacement -> ShipPlacement -> Bool
canPlaceShip placed placement =
  case getShipPositions placement of
    Nothing -> False
    Just positions ->
      not (any (\placement ->
         (any (hitShip placement) positions)
        ) placed)


updatePreparing : PrepareAction -> PrepareModel -> PrepareModel
updatePreparing action model =
  case action of
    PlaceShip placement ->
      if canPlaceShip model.placed placement then
        { model | placed = placement :: model.placed }
      else model
    SelectShip shipType orientation ->
      { model | selected = Just (shipType, orientation) }


initModel : GameModel
initModel = Preparing initPreparingModel


updateGameModel : GameModelAction -> GameModel -> GameModel
updateGameModel action model =
  case (model, action) of
    (Preparing prepareModel, Prepare prepareAction) ->
      let newModel = (updatePreparing prepareAction prepareModel)
      in
      if (length newModel.placed) == 5 then
        updateGameModel (PlayGame newModel.placed) model
      else Preparing newModel
    (_, PlayGame setup) ->
      Playing {setup = setup, missileLog = []}
    _ -> model


shipLength : ShipType -> Int
shipLength shipType =
  case shipType of
      AircraftCarrier -> 5
      Battleship -> 4
      Submarine -> 3
      Cruiser -> 3
      Patrol -> 2


shipName : ShipType -> String
shipName shipType =
  case shipType of
    AircraftCarrier -> "Aircraft Carrier"
    Battleship -> "Battleship"
    Cruiser -> "Cruiser"
    Submarine -> "Submarine"
    Patrol -> "Patrol Boat"



nowhere : Mailbox ()
nowhere = mailbox ()


discard : (a -> Message)
discard a =
  Signal.message nowhere.address ()
