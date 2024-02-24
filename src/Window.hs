{- |
Module      :  Window
Description :  This is the module which contains all the graphics and window
               configuration for the Monopoly game implementation.
-}

module Window
    (
        startGame
    ) where

import Graphics.Gloss
import Graphics.Gloss.Juicy

window = InWindow "Monopoly" (windowWidth, windowHeight) (10, 10)

windowWidth :: Int
windowWidth = 1117 

windowHeight :: Int
windowHeight = 1117

startGame :: IO ()
startGame = do
    maybePicture <- loadJuicyPNG "resources/board.png"

    case maybePicture of
        Just picture    -> display (InWindow "Display PNG" (1200, 1200) (10, 10)) white picture
        Nothing         -> putStrLn "Failed to load PNG file."
