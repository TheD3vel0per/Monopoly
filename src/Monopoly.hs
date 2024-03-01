{- |
Module      :  Monopoly
Description :  This is an implementation of the Monopoly game made by Devam
               Sisodraker and Kayla Oxland in requirement for the class CPSC 312
               offered at the University of British Columbia.
Copyright   :  (c) Devam Sisodraker, Kayla Oxland
License     :  MIT

Maintainer  :  devam@student.ubc.ca
Stability   :  unstable
Portability :  portable
-}
module Monopoly
    (
        PlayerID,
        NullablePlayerID,
        BoardLocation,
        GameState,
        PlayersState,
        PlayerState,
        BoardState,
        OwnableTileState,
        TileState,
        TurnState,
        currentLocation,
        findPlayerByID,
        replacePlayerState,
        findTileByLocation,
        replaceTileState,
        getDebugMessage,
        setDebugMessage,
        getDieResult,
        setDieResult,
        getDieRolled,
        setDieRolled,
        rollDie,
        incrementDieRollNumber,
        initialGameState,
        getPlayerStates,
        getPlayerID,
        getPlayerFunds
    ) where

import System.Random

--------------------------------
-- Data Definitions
--------------------------------

-- | Type for the Unique Identifier of a Player
type PlayerID = Int

-- | Type for the optional unique identification of a Player
type NullablePlayerID = Maybe PlayerID

-- | Type for identifying a position on the Board
type BoardLocation = Int

-- | Global state for the entire game
data GameState = GameState {
    playersState    :: PlayersState,        -- ^ Player state of all players in the game
    boardState      :: BoardState,          -- ^ Board state of the game
    turnState       :: TurnState            -- ^ State of the current turn
}

-- | Players state for the entire game
data PlayersState = PlayersState {
    playerStates    :: [PlayerState],       -- ^ List of all the players in the game
    playerIDTurn    :: PlayerID             -- ^ ID of the player whose turn it is
}

-- | Individual player state
data PlayerState = PlayerState {
    identifier      :: PlayerID,            -- ^ ID of the player
    funds           :: Int,                 -- ^ Amount of Money the Player Has
    currentLocation :: BoardLocation,       -- ^ Which tile the player is currently situated on
    propertiesOwned :: [BoardLocation]      -- ^ list of properties owned by the player
}

-- | Board state for the entire game
data BoardState = BoardState {
    tileUpperBound  :: BoardLocation,       -- ^ Largest identifier (in use) of tiles
    tilesState      :: [TileState]          -- ^ List of all the tiles on the board
}

-- | Tile state for an ownable tile on the board
data OwnableTileState = OwnableTileState {
    tileLocation    :: [BoardLocation],     -- ^ Location of the tile on the board
    tileOwner       :: PlayerID,            -- ^ Owner of the tile
    value           :: Int,                 -- ^ Value of the tile
    rent            :: Int                  -- ^ Rent to be paid when landing on this tile/property
}

-- | Tile state for an individual tile on the board
type TileState = OwnableTileState

-- | State of the turn in progress
data TurnState = TurnState {
    dieRollNumber   :: Int,                 -- ^ Which turn this current turn is
    debugMessage    :: String,              -- ^ Extra debugging message to be printed to the board
    diceRolled      :: Bool,                -- ^ Whether or not the dice has been rolled
    diceResult      :: (Int, Int)           -- ^ Resulting roll of the dice
}

--------------------------------
-- State Mutation
--------------------------------

-- | Find a player in the array by ID
findPlayerByID :: PlayerID -> [PlayerState] -> Maybe PlayerState
findPlayerByID _ [] = Nothing
findPlayerByID id (p:ps)
    | identifier p == id = Just p
    | otherwise = findPlayerByID id ps

-- | Function to update player state
replacePlayerState :: PlayerID -> PlayerState -> [PlayerState] -> [PlayerState]
replacePlayerState _ _ [] = []
replacePlayerState playerID newPlayerState (p:ps) =
    case findPlayerByID playerID (p:ps) of
        Just _ -> newPlayerState : ps
        Nothing -> p : replacePlayerState playerID newPlayerState ps

-- | Function to find an ownable tile by location on the board
findTileByLocation :: BoardLocation -> [OwnableTileState] -> Maybe OwnableTileState
findTileByLocation _ [] = Nothing
findTileByLocation location (tile:rest)
    | location `elem` tileLocation tile = Just tile
    | otherwise = findTileByLocation location rest

-- | Function to update tile state
replaceTileState :: BoardLocation -> OwnableTileState -> [OwnableTileState] -> [OwnableTileState]
replaceTileState _ _ [] = []
replaceTileState location newTileState (tile:rest) =
    case findTileByLocation location (tile:rest) of
        Just _ -> newTileState : rest
        Nothing -> tile : replaceTileState location newTileState rest

-- | Get the debug message from the game state
getDebugMessage :: GameState -> String
getDebugMessage gs = debugMessage (turnState gs)

-- | Set the debug message and produce a new game state
setDebugMessage :: String -> GameState -> GameState
setDebugMessage msg gs =
    gs { turnState = (turnState gs) { debugMessage = msg } }

-- | Get the die roll
getDieResult :: GameState -> (Int, Int)
getDieResult gs = diceResult $ turnState gs

-- | Set the die roll
setDieResult :: (Int, Int) -> GameState -> GameState
setDieResult pair gs =
    gs { turnState = (turnState gs) { diceResult = pair } }

-- | Get the die roll
getDieRolled :: GameState -> Bool
getDieRolled gs = diceRolled $ turnState gs

-- | Set the die roll
setDieRolled :: Bool -> GameState -> GameState
setDieRolled pair gs =
    gs { turnState = (turnState gs) { diceRolled = pair } }

-- | Roll the die
rollDie :: GameState -> GameState
rollDie gs =
    incrementDieRollNumber $
    setDieResult (dieRolls !! dieRollNumber (turnState gs)) gs

-- | Increment the die roll number
incrementDieRollNumber :: GameState -> GameState
incrementDieRollNumber gs =
    gs { turnState = (turnState gs) { dieRollNumber = 1 + dieRollNumber (turnState gs)}}

-- | Get the player states
getPlayerStates :: GameState -> [PlayerState]
getPlayerStates gs = playerStates $ playersState gs

-- | Get the player ID
getPlayerID :: PlayerState -> PlayerID
getPlayerID = identifier

-- | Get the player ID
getPlayerFunds :: PlayerState -> Int
getPlayerFunds = funds

--------------------------------
-- Definitions
--------------------------------
initialGameState :: GameState
initialGameState = GameState {
    playersState = PlayersState {
        playerStates = [
            PlayerState {
                identifier = 0, 
                funds = 10,
                currentLocation = 0,
                propertiesOwned = []
            },
            PlayerState {
                identifier = 1, 
                funds = 10,
                currentLocation = 0,
                propertiesOwned = []
            },
            PlayerState {
                identifier = 2, 
                funds = 10,
                currentLocation = 0,
                propertiesOwned = []
            },
            PlayerState {
                identifier = 3, 
                funds = 10,
                currentLocation = 0,
                propertiesOwned = []
            }
        ],
        playerIDTurn = 0
    },
    boardState = BoardState {
        tileUpperBound = 0,
        tilesState = []
    },
    turnState = TurnState {
        dieRollNumber = 0,
        debugMessage = "kemcho",
        diceRolled = False,
        diceResult = (2, 3)
    }
}

dieRollMax :: Int
dieRollMax = 250

dieRolls :: [(Int, Int)]
dieRolls = zip
    (map (((+) 1 . (`mod` 5)) . abs) (take dieRollMax $ randoms (mkStdGen 69)))
    (map (((+) 1 . (`mod` 5)) . abs) (take dieRollMax $ randoms (mkStdGen 420)))