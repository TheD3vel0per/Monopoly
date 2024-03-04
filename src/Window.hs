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
    translate (-265) 225 $ renderFunds players $ getPlayerStates gs,

    -- Ownership 
    renderOwnership $ getTileStates gs,

    -- Turn Indicator
    translate (-260) (-130) $ renderTurn gs,

    -- Buy & Pass Property Prompt
    translate (-5) (205 + 15) $ renderBuyAndPassPrompt (getPropertyBuyable gs) (currentPlayerCanBuy gs) (getCurrentPropertyName gs),
    
    -- Pay Rent Button
    translate 0 (205 + 20) $ renderPayRent $ getRentToBePayed gs,
    
    -- Next Turn Button
    translate 0 (205 - 20) $ renderNextTurn $ getTurnComplete gs,
    
    -- Legend
    translate 20 0 $ renderLegend players ]


renderRollButton :: Bool -> Picture
renderRollButton False = blank
renderRollButton True = pictures [
    color (greyN 0.5) $ rectangleSolid 150 30,
    translate (-60) (-5) $ color white $ scale 0.15 0.15 $ text "Roll the Die!" ]

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
mapPlayerToShift 0 pic = translate (-playerShift) playerShift pic
mapPlayerToShift 1 pic = translate playerShift playerShift pic
mapPlayerToShift 2 pic = translate (-playerShift) (-playerShift) pic
mapPlayerToShift 3 pic = translate playerShift (-playerShift) pic
mapPlayerToShift _ _ = blank

renderFunds :: [Picture] -> [PlayerState] -> Picture
renderFunds [] _ = blank
renderFunds _ [] = blank
renderFunds (pic:pics) (ps:pss) = pictures [
    let (x, y) = mapBoardLocation2Position4PlayerPos (currentLocation ps) in
        scale 0.125 0.125 $ text ("Player " ++ show (getPlayerID ps) ++ " has $" ++ show (getPlayerFunds ps)),
    translate 0 (-20) $ renderFunds pics pss ]

renderOwnership :: [TileState] -> Picture
renderOwnership [] = blank
renderOwnership (ts:tss) = pictures [
    case ts of
        OwnableTileState _ tid owner _ _ ->
            case owner of
                Just pid -> let (x, y) = mapBoardLocation2Position4OwnerPos tid in
                    translate x y $ scale 0.1 0.1 $ text ("Owned by Player " ++ show pid)
                Nothing -> Blank
        OtherTileState _ _ -> blank,
    renderOwnership tss ]

mapBoardLocation2Position4OwnerPos :: BoardLocation -> (Float, Float)
mapBoardLocation2Position4OwnerPos 1 = (-4, -380)
mapBoardLocation2Position4OwnerPos 2 = (-265, -380)
mapBoardLocation2Position4OwnerPos 4 = (-500, 0)
mapBoardLocation2Position4OwnerPos 5 = (-500, 260)
mapBoardLocation2Position4OwnerPos 7 = (-260, 360)
mapBoardLocation2Position4OwnerPos 8 = (25, 360)
mapBoardLocation2Position4OwnerPos 10 = (360, 240)
mapBoardLocation2Position4OwnerPos 11 = (360, -30)
mapBoardLocation2Position4OwnerPos _ = (0, 0)

renderTurn :: GameState -> Picture
renderTurn gs = let pid = getCurrentPlayerID gs in
    scale 0.25 0.25 $ text ("It's Player " ++ show pid ++ "'s turn.")

renderBuyAndPassPrompt :: Bool -> Bool -> String -> Picture
renderBuyAndPassPrompt False _ _ = blank
renderBuyAndPassPrompt True False tileName = pictures [
    translate (-70) 30 $ scale 0.125 0.125 $ text tileName,
    translate (80 - 35) 0 $ color (greyN 0.5) $ rectangleSolid 70 30,
    translate 20 (-5) $ color white $ scale 0.15 0.15 $ text "Pass" ]
renderBuyAndPassPrompt True True tileName = pictures [
    translate (-70) 30 $ scale 0.125 0.125 $ text tileName,
    translate (-35) 0 $ color (greyN 0.5) $ rectangleSolid 70 30,
    translate (-60) (-5) $ color white $ scale 0.15 0.15 $ text "Buy",
    translate (80 - 35) 0 $ color (greyN 0.5) $ rectangleSolid 70 30,
    translate 20 (-5) $ color white $ scale 0.15 0.15 $ text "Pass" ]
renderBuyAndPassPrompt _ _ _ = blank

renderPayRent :: Bool -> Picture
renderPayRent False = blank
renderPayRent True = pictures [
    color (greyN 0.5) $ rectangleSolid 150 30,
    translate (-60) (-5) $ color white $ scale 0.15 0.15 $ text "Pay Rent" ]

renderNextTurn :: Bool -> Picture
renderNextTurn False = blank
renderNextTurn True = pictures [
    color (greyN 0.5) $ rectangleSolid 150 30,
    translate (-60) (-5) $ color white $ scale 0.15 0.15 $ text "Next Turn" ]

renderLegend :: [Picture] -> Picture
-- renderLegend _ = blank
renderLegend players = pictures [
    translate (-250) (-180) $ players !! 0,
    translate (-220) (-180) $ scale 0.125 0.125 $ text "Player 0",
    translate (0) (-180) $ players !! 1,
    translate (40) (-180) $ scale 0.125 0.125 $ text "Player 1",
    translate (-260) (-240) $ players !! 2,
    translate (-220) (-240) $ scale 0.125 0.125 $ text "Player 2",
    translate (0) (-250) $ players !! 3,
    translate (40) (-240) $ scale 0.125 0.125 $ text "Player 3" ]


--------------------------------
-- Events
--------------------------------

onEventGame :: Event -> GameState -> GameState
onEventGame e gs =
    case e of
        EventKey key _ _ point ->
            case key of
                MouseButton _ -> onClick point gs
                _ -> gs
        _ -> gs

onClick :: (Float, Float) -> GameState -> GameState
onClick (x, y) gs
    -- Clicked on the Roll Dice Button
    | not (getDieRolled gs) && x >= 110.0 && y >= 150.0 && x <= 260.0 && y <= 180.0 =
        advanceCurrentPlayer $
        setDieRolled True $
        rollDie $
        setDebugMessage ("(" ++ show x ++ "," ++ show y ++ ")") gs

    -- Clicked on the Buy Button
    | currentPlayerCanBuy gs && getPropertyBuyable gs && x >= -75 && y >= 205 && x <= -5 && y <= 235 =
        currentPlayerBuy $ 
        setDebugMessage ("(" ++ show x ++ "," ++ show y ++ ") buy") gs

    -- Clicked on the Pass Button
    | getPropertyBuyable gs && x >= 5 && y >= 205 && x <= 75 && y <= 235 = 
        setTurnComplete True $
        setPropertyBuyable False $
        setDebugMessage ("(" ++ show x ++ "," ++ show y ++ ") pass") gs

    -- Clicked on the Pay Rent Button
    | getRentToBePayed gs && x >= -70 && y >= 210 && x <= 80 && y <= 240 = 
        currentPlayerPayRent $
        setDebugMessage ("(" ++ show x ++ "," ++ show y ++ ") pay") gs

    -- Clicked on Next Turn Button
    | getTurnComplete gs && x >= -70 && y >= 170 && x <= 80 && y <= 200 = 
        finishTurn $
        setDebugMessage ("(" ++ show x ++ "," ++ show y ++ ") next") gs

    -- Useless click
    | otherwise = setDebugMessage ("(" ++ show x ++ "," ++ show y ++ ")") gs