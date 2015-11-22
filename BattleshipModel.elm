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


type GameModel
  = Preparing (List ShipPlacement)
  | Playing (List ShipPlacement, MissileLog)


type PrepareAction
  = PlaceShip ShipType GridPosition
  | RotateShip


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
