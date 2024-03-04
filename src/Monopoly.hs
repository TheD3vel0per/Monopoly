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
        getPlayerFunds,
        getTileLocation,
        getTileStates,
        getCurrentPlayerID,
        getPropertyBuyable,
        setPropertyBuyable,
        advanceCurrentPlayer,
        getCurrentPropertyName,
        getRentToBePayed,
        setRentToBePayed,
        getTurnComplete,
        setTurnComplete,
        currentPlayerPayRent,
        currentPlayerBuy,
        currentPlayerCanBuy,
        finishTurn
    ) where

import System.Random
import qualified Data.Maybe

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
    tilesState   :: [TileState]          -- ^ List of all the tiles on the board
}

-- | Tile state for an individual tile on the board
data TileState = OwnableTileState {
        name            :: String,              -- ^ Name of the property
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
    diceResult      :: (Int, Int),          -- ^ Resulting roll of the dice
    propertyBuyable :: Bool,                -- ^ Can buy property
    rentToBePayed   :: Bool,                -- ^ Whether we have to pay rent on the current tile
    turnComplete    :: Bool                 -- ^ Turn can be completed
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
replacePlayerState playerID newPlayerState (p:ps)
    | identifier p == playerID = newPlayerState:ps
    | otherwise = p : replacePlayerState playerID newPlayerState ps

-- | Function to update player state
replaceTileState :: BoardLocation -> TileState -> [TileState] -> [TileState]
replaceTileState _ _ [] = []
replaceTileState tileID newTileState (t:ts)
    | tileLocation t == tileID = newTileState : ts
    | otherwise = t : replaceTileState tileID newTileState ts

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

-- | Get whether or not a property is buyable
getPropertyBuyable :: GameState -> Bool
getPropertyBuyable gs = propertyBuyable $ turnState gs

-- | Get whether or not a property is buyable
setPropertyBuyable :: Bool -> GameState -> GameState
setPropertyBuyable b gs = gs {
    turnState = (turnState gs) {
        propertyBuyable = b
    }
}
-- | Advance the current player
advanceCurrentPlayer :: GameState -> GameState
advanceCurrentPlayer gs =
    -- TODO add delta for any tile we land on negative tiles
    -- TODO add +20 for any time we pass go
    gs {
        playersState = ps {
            playerStates = replacePlayerState pid newPs pps
        },
        turnState = ts {
            rentToBePayed = newRentToBePayed,
            propertyBuyable = newPropertyBuyable,
            turnComplete = newTurnComplete
        }
    } where
        GameState ps bs ts = gs
        TurnState _ _ _ diceResult _ _ _ = ts
        BoardState tileModulus tts = bs
        PlayersState pps pid = ps
        PlayerState _ _ currentLocation _ = pps !! pid
        (diceOne, diceTwo) = diceResult
        newLocation = (currentLocation + diceOne + diceTwo) `mod` tileModulus
        newPs = (pps !! pid) { 
            currentLocation = newLocation,
            funds = fundsDelta + funds (pps !! pid) 
        }
        tile = tts !! newLocation
        newRentToBePayed = case tile of
            OwnableTileState _ _ maybeOwner _ _ ->
                case maybeOwner of
                    Just owner -> owner /= pid
                    Nothing -> False
            OtherTileState _ _ -> False
        newPropertyBuyable = case tile of
            OwnableTileState _ _ maybeOwner _ _ ->
                case maybeOwner of
                    Just _ -> False
                    Nothing -> True
            OtherTileState _ _ -> False
        newTurnComplete = case tile of
            OwnableTileState _ _ maybeOwner _ _ ->
                case maybeOwner of
                    Just owner -> owner == pid
                    Nothing -> False
            OtherTileState {} -> True
        passedGo = currentLocation >= newLocation
        fundsDelta = case tile of
            OwnableTileState {} -> 0 + if passedGo then 5 else 0
            OtherTileState _ fundDelta -> fundDelta + if passedGo then 5 else 0

-- | Get current property name
getCurrentPropertyName :: GameState -> String
getCurrentPropertyName gs =
    case tile of
        OwnableTileState tileName _ _ _ _ -> tileName
        OtherTileState {} -> ""
    where
        pid = playerIDTurn $ playersState gs
        ps = playerStates (playersState gs) !! pid
        tid = currentLocation ps
        tile = tilesState (boardState gs) !! tid

-- | Get whether or not a property has rent to be payed
getRentToBePayed :: GameState -> Bool
getRentToBePayed gs = rentToBePayed $ turnState gs

-- | Set whether or not a property has rent to be payed
setRentToBePayed :: Bool -> GameState -> GameState
setRentToBePayed b gs = gs {
    turnState = (turnState gs) {
        rentToBePayed = b
    }
}

-- | Get whether or not the turn can be completed
getTurnComplete :: GameState -> Bool
getTurnComplete gs = turnComplete $ turnState gs

-- | Set whether or not the turn can be completed
setTurnComplete :: Bool -> GameState -> GameState
setTurnComplete b gs = gs {
    turnState = (turnState gs) {
        turnComplete = b
    }
}

-- | Current player pays rent
currentPlayerPayRent :: GameState -> GameState
currentPlayerPayRent gs =
    gs {
        playersState = (playersState gs) {
            playerStates =
                replacePlayerState renterID (renterPS { funds = renterFunds - rentCost }) $
                replacePlayerState ownerID (ownerPS { funds = ownerFunds + rentCost }) $
                playerStates $ playersState gs
        },
        turnState = (turnState gs) {
            rentToBePayed = False,
            turnComplete = True
        }
    } where
        renterID = playerIDTurn $ playersState gs
        renterPS = playerStates (playersState gs) !! renterID
        renterFunds = funds renterPS
        tid = currentLocation $ playerStates (playersState gs) !! renterID
        tile = tilesState (boardState gs) !! tid
        ownerID = Data.Maybe.fromMaybe (- 1) (tileOwner tile)
        ownerPS = playerStates (playersState gs) !! ownerID
        ownerFunds = funds ownerPS
        rentCost = rent tile

-- | Current player pays rent
currentPlayerBuy :: GameState -> GameState
-- currentPlayerBuy gs = gs
currentPlayerBuy gs =
    gs {
        playersState = (playersState gs) {
            playerStates =
                replacePlayerState pid updatedPlayerState $
                playerStates $ playersState gs
        },
        boardState = (boardState gs) {
            tilesState =
                replaceTileState tid updatedTileState $
                tilesState $ boardState gs
        },
        turnState = (turnState gs) {
            propertyBuyable = False,
            rentToBePayed = False,
            turnComplete = True
        }
    } where
        pid = playerIDTurn $ playersState gs
        pps = playerStates (playersState gs) !! pid
        tid = currentLocation $ playerStates (playersState gs) !! pid
        tile = tilesState (boardState gs) !! tid
        cost = value tile
        updatedPlayerState = pps {
            propertiesOwned = tid : propertiesOwned pps,
            funds = funds pps - cost
        }
        updatedTileState = tile {
            tileOwner = Just pid
        }

currentPlayerCanBuy :: GameState -> Bool
-- currentPlayerCanBuy _ = True
currentPlayerCanBuy gs =
    case tile of
        OwnableTileState {} -> canBuy
        OtherTileState {} -> False
    where
        pid = playerIDTurn $ playersState gs
        pps = playerStates (playersState gs) !! pid
        tid = currentLocation $ playerStates (playersState gs) !! pid
        tile = tilesState (boardState gs) !! tid
        cost = value tile
        canBuy = cost < funds pps

-- | Current player finishes turn
finishTurn :: GameState -> GameState
finishTurn gs =
    gs {
        playersState = (playersState gs) {
            playerIDTurn = (1 + playerIDTurn (playersState gs)) `mod` length (playerStates $ playersState gs)
        },
        turnState = (turnState gs) {
            dieRollNumber = 1 + dieRollNumber (turnState gs),
            debugMessage = "kemcho",
            diceRolled = False,
            propertyBuyable = False,
            rentToBePayed = False,
            turnComplete = False
        }
    }

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
                name = "Agronomy Road",
                tileLocation = 1,
                tileOwner = Nothing,
                value = 1,
                rent = 1
            },
            OwnableTileState {          -- Wesbrook Mall
                name = "Wesbrook Mall",
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
                name = "Health Sciences Road",
                tileLocation = 4,
                tileOwner = Nothing,
                value = 5,
                rent = 3
            },
            OwnableTileState {          -- Engineering Road
                name = "Engineering Road",
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
                name = "Iona Road",
                tileLocation = 7,
                tileOwner = Nothing,
                value = 9,
                rent = 5
            },
            OwnableTileState {          -- Wesbrook Mall
                name = "Wesbrook Mall",
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
                name = "University Boulevard",
                tileLocation = 10,
                tileOwner = Nothing,
                value = 13,
                rent = 7
            },
            OwnableTileState {          -- Main Mall
                name = "Main Mall",
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
        diceResult = (2, 3),
        propertyBuyable = False,
        rentToBePayed = False,
        turnComplete = False
    }
}

dieRollMax :: Int
dieRollMax = 250

dieRolls :: [(Int, Int)]
dieRolls = zip
    (map (((+) 1 . (`mod` 5)) . abs) (take dieRollMax $ randoms (mkStdGen 234)))
    (map (((+) 1 . (`mod` 5)) . abs) (take dieRollMax $ randoms (mkStdGen 443)))