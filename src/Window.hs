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

import Monopoly

window = InWindow "Monopoly" (windowWidth, windowHeight) (0, 0)

windowTitle :: String
windowTitle = "Monopoly"

windowWidth :: Int
windowWidth = 1117 

windowHeight :: Int
windowHeight = 1117

gameRefreshRate :: Int
gameRefreshRate = 60

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
