module BattleshipModel where

import List exposing ((::))

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
    selected: ShipType,
    orientation: Orientation
  }


type GameModel
  = Preparing PrepareModel
  | Playing (List ShipPlacement, MissileLog)


type PrepareModelAction
  = PlaceShip ShipType Orientation GridPosition


type GameModelAction
  = NoOp
  | PrepareAction PrepareModelAction


initPreparingModel : PrepareModel
initPreparingModel  =
  { placed = [],
    selected = AircraftCarrier,
    orientation =  Horizontal
  }


updatePreparing : PrepareModelAction -> PrepareModel -> PrepareModel
updatePreparing action model =
  case action of
    PlaceShip shipType orientation gridPosition ->
      { model | placed = (shipType, gridPosition, orientation) :: model.placed }


initModel : GameModel
initModel = Preparing initPreparingModel


updateGameModel : GameModelAction -> GameModel -> GameModel
updateGameModel action model =
  case (action, model) of
    (NoOp, _) -> model
    (PrepareAction prepareModelAction, Preparing prepareModel) ->
      Preparing (updatePreparing prepareModelAction prepareModel)
    _ ->
      Debug.log ("illegal action " ++ (toString action) ++ " on " ++ (toString model))
      model


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
