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


--------------------------------
-- Data Definitions
--------------------------------


-- | Type for the Unique Identifier of a Player
type PlayerID = Int

-- | Type for the opetional unique identification of a Player
type NullablePlayerID = PlayerID | 0

-- | Type for identifying a position on the Board
type BoardLocation = Int

-- | Global state for the entire game
data GameState = GameState 
    PlayersState    -- ^ Player state of all players in the game
    BoardState      -- ^ Board state of the game

-- | Players state for the entire game
data PlayersState = PlayersState
    [PlayerState]   -- ^ List of all the players in the game
    PlayerID        -- ^ ID of the player whose turn it is

-- | Individual player state
data PlayerState = PlayerState
    PlayerID        -- ^ ID of the player
    Int             -- ^ Amount of Money the Player Has
    BoardLocation   -- ^ Which tile the player is currently situated on
    -- TODO @kaylaox fill in the rest of what player state is here

-- | Board state for the entire game
data BoardState = BoardState
    [TileState]     -- ^ List of all the tiles on the board
    BoardLocation   -- ^ Largest identifier (in use) of tiles
    -- TODO @kaylaox fill in the rest of what board state is here

-- | Tile state for an individual tile on the board
data TileState = TileState
    [BoardLocation]   -- ^ Location of the tile on the board
    PlayerID
    -- TODO @kaylaox fill in the rest of what tile state is here


--------------------------------
-- State Mutation
--------------------------------


--------------------------------
-- Game Turn
--------------------------------


--------------------------------
-- Graphics
--------------------------------