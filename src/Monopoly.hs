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
        replaceTileState
    ) where


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

-- | Tile state for an individual tile on the board
type TileState = OwnableTileState

-- | State of the turn in progress
data TurnState = TurnState {
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

--------------------------------
-- Game Turn
--------------------------------


--------------------------------
-- Graphics
--------------------------------