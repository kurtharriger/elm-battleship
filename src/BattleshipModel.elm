module BattleshipModel where

import List exposing ((::), length, map, any)
import Signal exposing (Mailbox, Message, mailbox)
import Random
import Time


type ShipType
  = AircraftCarrier
  | Battleship
  | Submarine
  | Cruiser
  | Patrol


allShips : List ShipType
allShips = [AircraftCarrier, Battleship, Cruiser, Submarine, Patrol]


shipName : ShipType -> String
shipName shipType =
  case shipType of
    AircraftCarrier -> "Aircraft Carrier"
    Battleship -> "Battleship"
    Cruiser -> "Cruiser"
    Submarine -> "Submarine"
    Patrol -> "Patrol Boat"


shipLength : ShipType -> Int
shipLength shipType =
  case shipType of
      AircraftCarrier -> 5
      Battleship -> 4
      Submarine -> 3
      Cruiser -> 3
      Patrol -> 2


type Orientation
  = Horizontal
  | Vertical


type alias GridPosition
  = (Int, Int)


type alias ShipPlacement
  = (ShipType, GridPosition, Orientation)


shipPlacement : ShipType -> Orientation -> GridPosition -> ShipPlacement
shipPlacement shipType orientation gridPosition =
  (shipType, gridPosition, orientation)


type MissileResult
  = Hit
  | Miss


type alias MissileLogEntry =
  {
    position : GridPosition,
    result : MissileResult
  }


type alias MissileLog = List MissileLogEntry


logMissile : GridPosition -> MissileResult -> MissileLogEntry
logMissile position result = MissileLogEntry position result


type GameModelState
  = Preparing
  | Playing
  | GameOver Winner


type alias GameModel = {
  state : GameModelState,
  selected: Maybe (ShipType, Orientation),
  setup: List ShipPlacement,
  missileLog: MissileLog,
  opposingSetup: List ShipPlacement,
  opposingMissileLog: MissileLog,
  seed : Random.Seed
}


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


type Winner
  = Player
  | Opponent


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

missileResult : List ShipPlacement -> GridPosition -> MissileResult
missileResult setup pos =
  if any (flip hitShip pos) setup then Hit else Miss


isHit : MissileResult -> Bool
isHit result =
  case result of
    Hit -> True
    Miss -> False


updatePreparing : PrepareAction -> GameModel -> GameModel
updatePreparing action model =
  case action of
    PlaceShip placement ->
      if canPlaceShip model.setup placement then
        { model | setup = placement :: model.setup }
      else model
    SelectShip shipType orientation ->
      { model | selected = Just (shipType, orientation) }


initModel : Random.Seed -> GameModel
initModel seed =
  let (opposingSetup, seed) = randomPositionings seed
  in
  {
    seed = seed,
    opposingSetup = opposingSetup,
    setup = [],
    selected = Nothing,
    state = Preparing,
    missileLog = [],
    opposingMissileLog = []
  }


isAlreadyLogged : GridPosition -> MissileLog  -> Bool
isAlreadyLogged pos =
  any (\{position} -> position == pos)


fireOpponentMissile : GameModel -> GameModel
fireOpponentMissile model =
  let (pos, seed) = Random.generate randomGridPosition model.seed
  in
  if isAlreadyLogged pos model.opposingMissileLog then
    fireOpponentMissile { model | seed = seed }
  else
    let opposingMissileLog = (logMissile pos <| missileResult model.setup pos) ::  model.opposingMissileLog
        destroyed = isPlayerDestroyed opposingMissileLog
    in
      { model |
        seed = seed,
        opposingMissileLog = opposingMissileLog,
        state = if destroyed then GameOver Opponent else model.state
      }



isPlayerDestroyed : MissileLog -> Bool
isPlayerDestroyed missileLog =
    let hitsRequired = List.sum <| map shipLength allShips
        hits = List.length <| List.filter (.result >> isHit) missileLog
    in
    hits >= hitsRequired


updateGameModel : GameModelAction -> GameModel -> GameModel
updateGameModel action model =
  -- let _ = (Debug.log "action" action)
  -- in
  case (model.state, action) of
    (Preparing, Prepare prepareAction) ->
      let (model) = updatePreparing prepareAction model
      in
      if (length model.setup) == 5 then
        updateGameModel (PlayGame model.setup) model
      else model
    (Preparing, PlayGame setup) ->
      {model | setup = setup, state = Playing}
    (Playing, Play (Fire pos)) ->
      if isAlreadyLogged pos model.missileLog then
        model
      else
        let missileLog = (logMissile pos <| missileResult model.opposingSetup pos) :: model.missileLog
            destroyed = isPlayerDestroyed model.missileLog
            state = if destroyed then GameOver Player else model.state
            newModel = { model | missileLog = missileLog, state = state }
        in
          if destroyed then
            newModel
          else
              fireOpponentMissile newModel
    _ -> model


nowhere : Mailbox ()
nowhere = mailbox ()


discard : (a -> Message)
discard a =
  Signal.message nowhere.address ()


randomOrientation : Random.Generator Orientation
randomOrientation =
  (Random.map
    (\b -> if b then Horizontal else Vertical)
    (Random.bool))

randomGridPosition : Random.Generator (Int, Int)
randomGridPosition =
  (Random.pair (Random.int 0 9) (Random.int 0 9))


randomPlacement : ShipType -> Random.Generator ShipPlacement
randomPlacement shipType =
  (Random.map2
    (\pos orientation -> shipPlacement shipType orientation pos)
    randomGridPosition
    (randomOrientation))


-- todo: try to rewrite this as generator?
randomValidPlacement : ShipType -> (List ShipPlacement, Random.Seed) -> (List ShipPlacement, Random.Seed)
randomValidPlacement shipType (current, seed) =
  let  (placement,seed) = Random.generate (randomPlacement shipType) seed
  in
  case (canPlaceShip current placement) of
    True -> (placement :: current,seed)
    False -> randomValidPlacement shipType (current, seed)


randomPositionings : Random.Seed -> (List ShipPlacement, Random.Seed)
randomPositionings seed =
    List.foldl randomValidPlacement ([],seed) allShips


initialSeed : Signal Random.Seed
initialSeed =
  Signal.map
    (\(time, _) -> Random.initialSeed (round time))
    (Time.timestamp (Signal.constant ()))
