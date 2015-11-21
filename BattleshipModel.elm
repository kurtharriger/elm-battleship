module BattleshipModel where

type ShipType = AircraftCarrier
              | Battleship
              | Submarine
              | Cruiser
              | Patrol


type Orientation = Horizontal
                 | Vertical


type alias GridPosition = (Int, Int)


type alias ShipPlacement = (ShipType, GridPosition, Orientation)


type LogEntry = Hit GridPosition
              | Miss GridPosition


type alias MissleLog = List LogEntry


shipLength : ShipType -> Int
shipLength shipType =
  case shipType of
      AircraftCarrier -> 5
      Battleship -> 4
      Submarine -> 3
      Cruiser -> 3
      Patrol -> 2
