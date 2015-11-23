module BattleshipModel where

import List exposing ((::), length)

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

type alias PlayModel = (List ShipPlacement, MissileLog)

type GameModel
  = Preparing PrepareModel
  | Playing PlayModel


type PrepareModelAction
  = PlaceShip ShipType Orientation GridPosition
  | SelectShip ShipType Orientation
  | PrepareNoOp


type GameModelAction
  = NoOp
  | PrepareAction PrepareModel
  | PlayGame PlayModel


initPreparingModel : PrepareModel
initPreparingModel  =
  { placed = [],
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


updatePreparing : PrepareModelAction -> PrepareModel -> PrepareModel
updatePreparing action model =
  case action of
    PlaceShip shipType orientation gridPosition ->
      { model | placed = (shipType, gridPosition, orientation) :: model.placed }
    SelectShip shipType orientation ->
      { model | selected = Just (shipType, orientation) }
    PrepareNoOp -> model


initModel : GameModel
initModel = Preparing initPreparingModel


updateGameModel : GameModelAction -> GameModel -> GameModel
updateGameModel action model =
  case action of
    PrepareAction prepareModel -> Preparing prepareModel
    PlayGame playModel -> Playing playModel
    NoOp -> model


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
