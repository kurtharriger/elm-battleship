module BattleshipModel where

import Signal exposing (Mailbox, Address, Message, mailbox)

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
    selected: (ShipType, Orientation)
  }


type GameModel
  = Preparing PrepareModel
  | Playing (List ShipPlacement, MissileLog)


type PrepareModelAction
  = PrepareModelActionNoOp
  | PlaceShip ShipType GridPosition


type GameModelAction
  = NoOp
  | PrepareAction PrepareModelAction


initPreparingModel : PrepareModel
initPreparingModel  =
  { placed = [],
    selected = (AircraftCarrier, Horizontal)
  }


-- updatePreparing action model =
--   case action of ->
--     PlaceShip shipType gridPosition ->
--       Preparing ()


initModel : GameModel
initModel = Preparing initPreparingModel


updateGameModel : GameModelAction -> GameModel -> GameModel
updateGameModel action model =
  Debug.log (toString action)
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


nowhere : Signal.Mailbox (Maybe ())
nowhere = (mailbox Nothing)


doNothing : Address a
doNothing = (Signal.forwardTo nowhere.address (always Nothing))
