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
        TileState ( OwnableTileState, OtherTileState ),
        TurnState,
        currentLocation,
        findPlayerByID,
        replacePlayerState,
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
        getPlayerFunds,
        getTileLocation,
        getTileStates,
        getCurrentPlayerID
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
    tileModulus  :: BoardLocation,       -- ^ Largest identifier (in use) of tiles
    tilesState      :: [TileState]          -- ^ List of all the tiles on the board
}

-- | Tile state for an individual tile on the board
data TileState = OwnableTileState {
        tileLocation    :: BoardLocation,       -- ^ Location of the tile on the board
        tileOwner       :: Maybe PlayerID,      -- ^ Owner of the tile
        value           :: Int,                 -- ^ Value of the tile
        rent            :: Int                  -- ^ Rent to be paid when landing on this tile/property
    } | OtherTileState {
        tileLocation    :: BoardLocation,       -- ^ Location of the tile on the board
        fundDelta       :: Int                  -- ^ How much this tile changes the funds of the player who lands on it
    }

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
getPlayerFunds :: PlayerState -> PlayerID
getPlayerFunds = funds

-- | Get tile location
getTileLocation :: TileState -> BoardLocation
getTileLocation = tileLocation

-- | Get the tile states
getTileStates :: GameState -> [TileState]
getTileStates gs = tilesState (boardState gs)

-- | Get the ID of the current players (whose turn it is)
getCurrentPlayerID :: GameState -> PlayerID
getCurrentPlayerID gs = playerIDTurn (playersState gs)

-- | Update the GameState when someones presses the buy button 
currentPlayerBuy :: GameState -> GameState
currentPlayerBuy gameState@(GameState playersState _ _) = 
    let currentPlayerID = playerIDTurn playersState
        newPlayers = updatePlayerState currentPlayerID (buyPropertyForPlayer currentPlayerID (playerStates playersState))
    in gameState { playersState = newPlayers }

-- | Update the GameState when someone presses the pass button 
currentPlayerPass :: GameState -> GameState
currentPlayerPass gameState@(GameState playersState _ _) = 
    let currentPlayerID = playerIDTurn playersState
        newPlayerIDTurn = getNextPlayerIDTurn currentPlayerID (playerStates playersState)
    in gameState { playersState = playersState { playerIDTurn = newPlayerIDTurn } }

-- | Buy a property for a player
buyPropertyForPlayer :: PlayerID -> TileState -> [PlayerState] -> [PlayerState]
buyPropertyForPlayer _ _ [] = []
buyPropertyForPlayer targetID property (player:players)
    | identifier player == targetID =
        let newProperties = property : propertiesOwned player
        in player { propertiesOwned = newProperties } : players
    | otherwise = player : buyPropertyForPlayer targetID property players

-- | Update a player state
updatePlayerState :: PlayerID -> [PlayerState] -> PlayersState
updatePlayerState _ [] = error "Player not found."
updatePlayerState targetID (player:players)
    | identifier player == targetID = PlayersState (player : players) targetID
    | otherwise = updatePlayerState targetID players

-- | Get the next player ID for next turn\
getNextPlayerIDTurn :: PlayerID -> [PlayerState] -> PlayerID
getNextPlayerIDTurn _ [] = error "Player not found."
getNextPlayerIDTurn currentID players = 
    let (nextPlayer:_) = dropWhile (\player -> identifier player /= currentID) (tail (cycle players))
    in identifier nextPlayer

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
        tileModulus = 12,
        tilesState = [
            OtherTileState {            -- Go
                tileLocation = 0,
                fundDelta = 20
            },
            OwnableTileState {          -- Agronomy Road
                tileLocation = 1,
                tileOwner = Nothing,
                value = 1,
                rent = 1
            },
            OwnableTileState {          -- Wesbrook Mall
                tileLocation = 2,
                tileOwner = Nothing,
                value = 3,
                rent = 2
            },
            OtherTileState {            -- Free
                tileLocation = 3,
                fundDelta = 0
            },
            OwnableTileState {          -- Health Sciences Road
                tileLocation = 4,
                tileOwner = Nothing,
                value = 5,
                rent = 3
            },
            OwnableTileState {          -- Engineering Road
                tileLocation = 5,
                tileOwner = Nothing,
                value = 7,
                rent = 4
            },
            OtherTileState {            -- Fine
                tileLocation = 6,
                fundDelta = -5
            },
            OwnableTileState {          -- Iona Road
                tileLocation = 7,
                tileOwner = Nothing,
                value = 9,
                rent = 5
            },
            OwnableTileState {          -- Wesbrook Mall
                tileLocation = 8,
                tileOwner = Nothing,
                value = 11,
                rent = 6
            },
            OtherTileState {            -- Free
                tileLocation = 9,
                fundDelta = 0
            },
            OwnableTileState {          -- University Boulevard
                tileLocation = 10,
                tileOwner = Nothing,
                value = 13,
                rent = 7
            },
            OwnableTileState {          -- Main Mall
                tileLocation = 11,
                tileOwner = Nothing,
                value = 15,
                rent = 8
            }
        ]
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