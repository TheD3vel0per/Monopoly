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
windowWidth = 1024

windowHeight :: Int
windowHeight = 1024

gameRefreshRate :: Int
gameRefreshRate = 60

playGame :: IO ()
playGame = do
    maybeBoard <- loadJuicyPNG "resources/board-mini.png"
    die <- loadImages ["resources/dice1.png", "resources/dice2.png", "resources/dice3.png", "resources/dice4.png", "resources/dice5.png", "resources/dice6.png"]
    players <- loadImages ["resources/player1.png", "resources/player2.png", "resources/player3.png", "resources/player4.png"]

    case maybeBoard of
        Just board -> play window white gameRefreshRate initialGameState (renderGame board die players) onEventGame tickGame
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

renderGame :: Picture -> [Picture] -> [Picture] -> GameState -> Picture
renderGame board die players gs = pictures [

    -- Board
    board,

    -- Debug Message
    translate (-265) 250 $ scale 0.125 0.125 $ text $ getDebugMessage gs,

    -- Dice
    translate 145 220 $ renderDie die $ getDieResult gs,

    -- Roll Button
    translate 185 165 $ renderRollButton $ not $ getDieRolled gs,

    -- Players
    renderPlayers players $ getPlayerStates gs,

    -- Funds
    translate (-265) 225 $ renderFunds players $ getPlayerStates gs ]

renderRollButton :: Bool -> Picture
renderRollButton False = blank
renderRollButton True = pictures [
    color (greyN 0.5) $ rectangleSolid 150 30,
    translate (-60) (-5) $ color white $ scale 0.15 0.15 $ text "Roll the Die!"
    ]

renderDie :: [Picture] -> (Int, Int) -> Picture
renderDie die (a, b) = pictures [
    renderDice die a,
    translate 80 0 $ renderDice die b ]

renderDice :: [Picture] -> Int -> Picture
renderDice die 1 = scale 0.3 0.3 $ die !! (1 - 1)
renderDice die 2 = scale 0.3 0.3 $ die !! (2 - 1)
renderDice die 3 = scale 0.3 0.3 $ die !! (3 - 1)
renderDice die 4 = scale 0.3 0.3 $ die !! (4 - 1)
renderDice die 5 = scale 0.3 0.3 $ die !! (5 - 1)
renderDice die 6 = scale 0.3 0.3 $ die !! (6 - 1)
renderDice _ _ = blank

renderPlayers :: [Picture] -> [PlayerState] -> Picture
renderPlayers [] _ = blank
renderPlayers _ [] = blank
renderPlayers (pic:pics) (ps:pss) = pictures [
    let (x, y) = mapBoardLocation2Position4PlayerPos (currentLocation ps) in
        translate x y $ mapPlayerToShift (getPlayerID ps) pic,
    renderPlayers pics pss ]

mapBoardLocation2Position4PlayerPos :: BoardLocation -> (Float, Float)
mapBoardLocation2Position4PlayerPos 0 = (405, -400)
mapBoardLocation2Position4PlayerPos 1 = (120, -440)
mapBoardLocation2Position4PlayerPos 2 = (-150, -440)
mapBoardLocation2Position4PlayerPos 3 = (-405, -400)
mapBoardLocation2Position4PlayerPos 4 = (-450, -120)
mapBoardLocation2Position4PlayerPos 5 = (-450, 150)
mapBoardLocation2Position4PlayerPos 6 = (-405, 400)
mapBoardLocation2Position4PlayerPos 7 = (-130, 430)
mapBoardLocation2Position4PlayerPos 8 = (130, 430)
mapBoardLocation2Position4PlayerPos 9 = (400, 400)
mapBoardLocation2Position4PlayerPos 10 = (440, 130)
mapBoardLocation2Position4PlayerPos 11 = (440, -130)
mapBoardLocation2Position4PlayerPos _ = (0, 0)

playerShift :: Float
playerShift = 30

mapPlayerToShift :: PlayerID -> Picture -> Picture
mapPlayerToShift 0 pic = translate playerShift (-playerShift) pic
mapPlayerToShift 1 pic = translate playerShift playerShift pic
mapPlayerToShift 2 pic = translate (-playerShift) (-playerShift) pic
mapPlayerToShift 3 pic = translate (-playerShift) playerShift pic
mapPlayerToShift _ _ = blank

renderFunds :: [Picture] -> [PlayerState] -> Picture
renderFunds [] _ = blank
renderFunds _ [] = blank
renderFunds (pic:pics) (ps:pss) = pictures [
    let (x, y) = mapBoardLocation2Position4PlayerPos (currentLocation ps) in
        scale 0.125 0.125 $ text ("Player " ++ show (getPlayerID ps) ++ " has $" ++ show (getPlayerFunds ps)),
    translate 0 (-20) $ renderFunds pics pss ]

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
    | x >= 110.0 && y >= 150.0 && x <= 260.0 && y <= 180.0 && not (getDieRolled gs) =
        rollDie $
        setDieRolled True $
        setDebugMessage ("(" ++ show x ++ "," ++ show y ++ ")") gs

    -- Useless click
    | otherwise = setDebugMessage ("(" ++ show x ++ "," ++ show y ++ ")") gs