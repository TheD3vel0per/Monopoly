{- |
Module      :  Window
Description :  This is the module which contains all the graphics and window
               configuration for the Monopoly game implementation.
-}

module Window
    (
        startGame,
        playGame
    ) where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Interact

-- import Monopoly

type GameState = Int

window = InWindow "Monopoly" (windowWidth, windowHeight) (0, 0)

windowTitle :: String
windowTitle = "Monopoly"

windowWidth :: Int
windowWidth = 1117 

windowHeight :: Int
windowHeight = 1117

gameRefreshRate :: Int
gameRefreshRate = 60

initialGameState :: GameState
initialGameState = 0
-- initialGameState = (GameState (PlayersState [] 0) (BoardState 0 []) (TurnState False (1, 1)))
-- initialGameState = GameState {
--     playersState = PlayersState {
--         playerStates = [],
--         playerIDTurn = 0
--     },
--     boardState = BoardState {
--         tileUpperBound = 0,
--         tilesState = []
--     },
--     turnState = TurnState {
--         diceRolled = False,
--         diceResult = (0, 0)
--     }
-- }
-- TODO

startGame :: IO ()
startGame = do
    maybePicture <- loadJuicyPNG "resources/board.png"

    case maybePicture of
        Just picture    -> display (InWindow "Display PNG" (1200, 1200) (10, 10)) white picture
        Nothing         -> putStrLn "Failed to load PNG file."

playGame :: IO ()
playGame = do
    maybeBoard <- loadJuicyPNG "resources/board.png"

    case maybeBoard of
        Just board  -> play window white gameRefreshRate initialGameState (renderGame board) onEventGame tickGame
        Nothing     -> putStrLn "Failed to load PNG file."

tickGame :: Float -> GameState -> GameState
tickGame f gs = gs

renderGame :: Picture -> GameState -> Picture
renderGame board gs = board

onEventGame :: Event -> GameState -> GameState
onEventGame e gs = initialGameState
