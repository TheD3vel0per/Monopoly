{- |
Module      :  Window
Description :  This is the module which contains all the graphics and window
               configuration for the Monopoly game implementation.
-}

module Window
    (
        playGame
    ) where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Interact

import Monopoly


--------------------------------
-- Window
--------------------------------

window :: Display
window = InWindow windowTitle (windowWidth, windowHeight) (0, 0)

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
    die <- loadImages ["resources/dice1.png", "resources/dice2.png", "resources/dice3.png", "resources/dice4.png", "resources/dice5.png", "resources/dice6.png"]

    case maybeBoard of
        Just board -> play window white gameRefreshRate initialGameState (renderGame board die) onEventGame tickGame
        Nothing -> putStrLn "Failed to load board file."

tickGame :: Float -> GameState -> GameState
tickGame _ gs = gs

--------------------------------
-- Rendering
--------------------------------

loadImages :: [String] -> IO [Picture]
loadImages imagePaths = do
    maybeImages <- mapM loadJuicy imagePaths
    case sequence maybeImages of
        Just images -> return images
        Nothing   -> return []

renderGame :: Picture -> [Picture] -> GameState -> Picture
renderGame board die gs = pictures [

    -- Board
    board,

    -- Debug Message
    translate (-400) (400) $ scale 0.125 0.125 $ text $ getDebugMessage gs,

    -- Dice
    translate 225 350 $ renderDie die $ getDieResult gs,

    -- Roll Button
    translate 285 250 $ renderRollButton $ not $ getDieRolled gs
    ]

renderRollButton :: Bool -> Picture
renderRollButton False = blank
renderRollButton True = pictures [
    color (greyN 0.5) $ rectangleSolid 220 50,
    translate (-100) (-10) $ color white $ scale 0.25 0.25 $ text "Roll the Die!"
    ]

renderDie :: [Picture] -> (Int, Int) -> Picture
renderDie die (a, b) = pictures [
    renderDice die a, 
    translate 120 0 $ renderDice die b ]

renderDice :: [Picture] -> Int -> Picture
renderDice die 1 = scale 0.5 0.5 $ die !! (1 - 1)
renderDice die 2 = scale 0.5 0.5 $ die !! (2 - 1)
renderDice die 3 = scale 0.5 0.5 $ die !! (3 - 1)
renderDice die 4 = scale 0.5 0.5 $ die !! (4 - 1)
renderDice die 5 = scale 0.5 0.5 $ die !! (5 - 1)
renderDice die 6 = scale 0.5 0.5 $ die !! (6 - 1)
renderDice _ _ = blank

--------------------------------
-- Events
--------------------------------

onEventGame :: Event -> GameState -> GameState
onEventGame e gs =
    case e of
        EventKey key _ _ point ->
            case key of
                MouseButton _ -> onClick point gs
                otherwise -> gs
        otherwise -> gs

onClick :: (Float, Float) -> GameState -> GameState
onClick (x, y) gs
    -- Clicked on the Roll Dice Button
    | (x >= 175.0) && (y >= 225.0) && (x <= 390.0) && (y <= 275.0) && not (getDieRolled gs) =
        rollDie $
        setDieRolled True $
        setDebugMessage ("(" ++ show x ++ "," ++ show y ++ ")") gs

    -- Useless click
    | otherwise = setDebugMessage ("(" ++ show x ++ "," ++ show y ++ ")") gs