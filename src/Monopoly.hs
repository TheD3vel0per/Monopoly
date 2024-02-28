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
        initialGameState
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
    inJail          :: Bool,                -- ^ whether the player is in jail or not
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

-- | Data type for the tiles
data Tile = Property { name :: String, cost :: Int, rent :: Int }
          | Free
          | Fine { fine :: Int}
          | Go 

-- | Defining the initial board state with tiles
initialBoard :: [Tile]
initialBoard = 
    [Go, 
     Property "Agronomy Rd" 1 1,
     Property "Wesbrook Mall" 3 2,
     Free,
     Property "Health Sciences Road" 5 3,
     Property "Engineering Road" 7 4,
     Fine 5,
     Property "Iona Drive" 9 5,
     Property "Wesbrook Mall" 11 6, 
     Free,
     Property "University Boulevard" 13 7,
     Property "Main Mall" 15 8]

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
    setDieResult (dieRolls !! (dieRollNumber (turnState gs))) gs

-- | Increment the die roll number
incrementDieRollNumber :: GameState -> GameState
incrementDieRollNumber gs =
    gs { turnState = (turnState gs) { dieRollNumber = 1 + dieRollNumber (turnState gs)}}

-- | convert Tile to OwnableTileState
tileToOwnableTileState :: BoardLocation -> Tile -> OwnableTileState
tileToOwnableTileState location tile = 
    case tile of 
        Property name cost rent -> OwnableTileState [location] 0 cost rent
                                -> OwnableTileState [location] 0 0 0 

-- | create the initial board state
initialBoardState :: BoardState
initialBoardState = BoardState(length initialBoard - 1) (zipWith tileToOwnableTileState [0..] initialBoard)

-- | function to initialize the game state 
initialGameState :: GameState
initialGameState = GameState initialPlayersState initialBoardState initialTurnState
    where 
        initialPlayersState = PlayersState [] 0 -- assuming there are no players initially
        initialTurnState = TurnState 0 "" False (0,0)

--------------------------------
-- Definitions
--------------------------------
initialGameState :: GameState
initialGameState = GameState {
    playersState = PlayersState {
        playerStates = [],
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
    (map ((+) 1) $ map (\x -> mod x 5) $ map abs $ take dieRollMax $ randoms (mkStdGen 69))
    (map ((+) 1) $ map (\x -> mod x 5) $ map abs $ take dieRollMax $ randoms (mkStdGen 420))