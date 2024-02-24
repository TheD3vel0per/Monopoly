main :: IO ()
main = do
    putStrLn "ABBA"

{-
import Test.HUnit
import Data.List (sort)

-- function for testChangeFundsFromPlayer
-- changeFundsFromPlayer :: PlayerID -> Int -> GameState -> GameState -- ?? not sure if it should be gamestate or PlayerDict being returned
changeFundsFromPlayer playerID amount playerDict = do
    player <- Map.lookup playerID playerDict
    let updatedPlayer = player {PlayerFunds = PlayerFunds player - amount}
    return $ Map.insert playerID updatedPlayer playerDict

-- Test Cases for ChangeFundsFromPlayer
testChangeFundsFromPlayer :: Test
testChangeFundsFromPlayer = TestCase $ assertEqual
    "Should subtract funds correctly"
    (PlayerState 01 500 (sort ["Property1", "Property2"]))
    (changeFundsFromPlayer2 (PlayerState 01 1000 ["Property1", "Property2"]) 500)

testChangeFundsFromPlayer :: Test
testChangeFundsFromPlayer = TestCase $ assertEqual
    "Should not subtract funds if amount is greater than player's funds"
    (PlayerState 02 100 ["Property1"])
    (changeFundsFromPlayer2 (PlayerState 02 100 ["Property1"]) 200)

-- Find a player in the array by ID
-- findPlayerByID :: PlayerID -> [PlayerState] -> Maybe PlayerState
findPlayerByID _ [] = Nothing
findPlayerByID id (p:ps)
    | identifier p == id = Just p
    | otherwise = findPlayerByID id ps

-- Test Cases for findPlayerByID
testFindPlayerByID :: Test
testFindPlayerByID = TestCase $ do
    let players = [PlayerState 1 "Alice", PlayerState 2 "Bob", PlayerState 3 "Charlie"]
    assertEqual "Should return Just PlayerState with ID 2" (Just (PlayerState 2 "Bob")) (findPlayerByID 2 players)
    assertEqual "Should return Just PlayerState with ID 1" (Just (PlayerState 1 "Alice")) (findPlayerByID 1 players)
    assertEqual "Should return Just PlayerState with ID 3" (Just (PlayerState 3 "Charlie")) (findPlayerByID 3 players)
    assertEqual "Should return Nothing for non-existing ID" Nothing (findPlayerByID 4 players)

-- Function to update player state
-- replacePlayerState :: PlayerID -> PlayerState -> [PlayerState] -> [PlayerState]
replacePlayerState _ _ [] = []
replacePlayerState playerID newPlayerState (p:ps) =
    case findPlayerByID playerID (p:ps) of
        Just _ -> newPlayerState : ps
        Nothing -> p : replacePlayerState playerID newPlayerState ps

-- Test Cases for replacePlayerState
testReplacePlayerState :: Test
testReplacePlayerState = TestCase $ do
    let players = [PlayerState 1 "Alice", PlayerState 2 "Bob", PlayerState 3 "Charlie"]
    
    -- Test case 1: Replace player with ID 2
    let updatedPlayers1 = replacePlayerState 2 (PlayerState 2 "Bobby") players
    assertEqual "Should replace player with ID 2" [PlayerState 1 "Alice", PlayerState 2 "Bobby", PlayerState 3 "Charlie"] updatedPlayers1
    
    -- Test case 2: Replace player with ID 1
    let updatedPlayers2 = replacePlayerState 1 (PlayerState 1 "Alice2") players
    assertEqual "Should replace player with ID 1" [PlayerState 1 "Alice2", PlayerState 2 "Bob", PlayerState 3 "Charlie"] updatedPlayers2
    
    -- Test case 3: Player ID not found, no replacement
    let updatedPlayers3 = replacePlayerState 4 (PlayerState 4 "David") players
    assertEqual "Should not replace anything if ID not found" players updatedPlayers3

-- Function to find an ownable tile by location on the board
-- findTileByLocation :: BoardLocation -> [OwnableTileState] -> Maybe OwnableTileState
findTileByLocation _ [] = Nothing
findTileByLocation location (tile:rest) = 
    | loc `elem` tileLocation tile = Just tile
    | otherwise = findTileByLocation location rest

-- Test Cases for findTileByLocation
testFindTileByLocation :: Test
testFindTileByLocation = TestCase $ do
    let tiles = [OwnableTileState [(1,1)], OwnableTileState [(2,2)], OwnableTileState [(3,3)]]
    
    -- Test case 1: Tile at location (2,2) exists
    assertEqual "Should find tile at location (2,2)" (Just (OwnableTileState [(2,2)])) (findTileByLocation (2,2) tiles)
    
    -- Test case 2: Tile at location (4,4) doesn't exist
    assertEqual "Should return Nothing if tile doesn't exist at location (4,4)" Nothing (findTileByLocation (4,4) tiles)
    
    -- Test case 3: Tile at location (1,1) exists
    assertEqual "Should find tile at location (1,1)" (Just (OwnableTileState [(1,1)])) (findTileByLocation (1,1) tiles)

-- Function to update tile state
-- replaceTileState :: BoardLocation -> OwnableTileState -> [OwnableTileState] -> [OwnableTileState]
replaceTileState _ _ [] = []
replaceTileState location newTileState (tile:rest) = 
    case findTileByLocation location (tile:rest) of 
        Just _ -> newTileState : rest
        Nothing -> tile : replaceTileState location newTileState rest

-- Test Cases for replaceTileState
testReplaceTileState :: Test
testReplaceTileState = TestCase $ do
    let tiles = [ OwnableTileState (1,1) "Owner1", OwnableTileState (2,2) "Owner2", OwnableTileState (3,3) "Owner3" ]
    
    -- Test case 1: Replace tile state at location (2,2)
    let updatedTiles1 = replaceTileState (2,2) (OwnableTileState (2,2) "NewOwner") tiles
    assertEqual "Should replace tile state at location (2,2)" 
                [OwnableTileState (1,1) "Owner1", OwnableTileState (2,2) "NewOwner", OwnableTileState (3,3) "Owner3"] 
                updatedTiles1
    
    -- Test case 2: Replace tile state at location (1,1)
    let updatedTiles2 = replaceTileState (1,1) (OwnableTileState (1,1) "NewOwner") tiles
    assertEqual "Should replace tile state at location (1,1)" 
                [OwnableTileState (1,1) "NewOwner", OwnableTileState (2,2) "Owner2", OwnableTileState (3,3) "Owner3"] 
                updatedTiles2
    
    -- Test case 3: Tile at location (4,4) doesn't exist, no replacement
    let updatedTiles3 = replaceTileState (4,4) (OwnableTileState (4,4) "Owner4") tiles
    assertEqual "Should not replace anything if tile doesn't exist at location (4,4)" 
                tiles 
                updatedTiles3


-}