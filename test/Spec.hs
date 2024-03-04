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

main :: IO ()
main = hspec $ do
    describe "currentPlayerBuy" $ do
        it "should buy a property for the current player" $ do
            let initialState = GameState
                    { playersState = PlayersState
                        { playerStates = [PlayerState 1 100 0 []]  -- Assuming only one player
                        , playerIDTurn = 1
                        }
                    , boardState = undefined  -- Not relevant for this test
                    , turnState = undefined   -- Not relevant for this test
                    }
                finalState = currentPlayerBuy initialState
            -- Assuming the property bought has a BoardLocation of 1
            propertiesOwned (head (playerStates (playersState finalState))) `shouldBe` [1]

    describe "currentPlayerPass" $ do
        it "should pass the turn to the next player" $ do
            let initialState = GameState
                    { playersState = PlayersState
                        { playerStates = [PlayerState 1 100 0 [], PlayerState 2 100 0 []]  -- Assuming two players
                        , playerIDTurn = 1
                        }
                    , boardState = undefined  -- Not relevant for this test
                    , turnState = undefined   -- Not relevant for this test
                    }
                finalState = currentPlayerPass initialState
            playerIDTurn (playersState finalState) `shouldBe` 2

        it "should wrap around to the first player after the last player" $ do
            let initialState = GameState
                    { playersState = PlayersState
                        { playerStates = [PlayerState 1 100 0 [], PlayerState 2 100 0 []]  -- Assuming two players
                        , playerIDTurn = 2
                        }
                    , boardState = undefined  -- Not relevant for this test
                    , turnState = undefined   -- Not relevant for this test
                    }
                finalState = currentPlayerPass initialState
            playerIDTurn (playersState finalState) `shouldBe` 1

main :: IO ()
main = hspec $ do
  describe "removeOwnership" $ do
    context "when a player owns properties" $ do
      it "removes ownership of all properties when a player goes bankrupt" $ do
        let initialPlayerState = PlayerState 1 100 0 [1, 2, 3]
            updatedPlayerState = removeOwnership initialPlayerState
        propertiesOwned updatedPlayerState `shouldBe` []

    context "when a player owns no properties" $ do
      it "does not change the list of owned properties" $ do
        let initialPlayerState = PlayerState 1 100 0 []
            updatedPlayerState = removeOwnership initialPlayerState
        propertiesOwned updatedPlayerState `shouldBe` []

  describe "clearOwnership" $ do
    context "when a player owns properties" $ do
      it "clears ownership of all properties when a player goes bankrupt" $ do
        let initialGameState = GameState (PlayersState [PlayerState 1 100 0 [1, 2, 3]] 1) (BoardState 3 []) (TurnState 1 "" False (0, 0) False False)
            updatedGameState = clearOwnership initialGameState
        propertiesOwned (head (playerStates (playersState updatedGameState))) `shouldBe` []

    context "when a player owns no properties" $ do
      it "does not change the list of owned properties" $ do
        let initialGameState = GameState (PlayersState [PlayerState 1 100 0 []] 1) (BoardState 3 []) (TurnState 1 "" False (0, 0) False False)
            updatedGameState = clearOwnership initialGameState
        propertiesOwned (head (playerStates (playersState updatedGameState))) `shouldBe` []

  describe "clearOwnershipFromTiles" $ do
    context "when there are tiles with ownership" $ do
      it "clears ownership of all tiles when a player goes bankrupt" $ do
        let initialGameState = GameState (PlayersState [PlayerState 1 100 0 [1, 2, 3]] 1) (BoardState 3 [OwnableTileState "Property1" 1 (Just 1) 100 10, OwnableTileState "Property2" 2 (Just 1) 200 20, OwnableTileState "Property3" 3 (Just 1) 300 30]) (TurnState 1 "" False (0, 0) False False)
            updatedGameState = clearOwnershipFromTiles initialGameState
        map tileOwner (tilesState (boardState updatedGameState)) `shouldBe` [Nothing, Nothing, Nothing]

    context "when there are no tiles with ownership" $ do
      it "does not change the ownership status of any tile" $ do
        let initialGameState = GameState (PlayersState [PlayerState 1 100 0 []] 1) (BoardState 3 [OwnableTileState "Property1" 1 Nothing 100 10, OwnableTileState "Property2" 2 Nothing 200 20, OwnableTileState "Property3" 3 Nothing 300 30]) (TurnState 1 "" False (0, 0) False False)
            updatedGameState = clearOwnershipFromTiles initialGameState
        map tileOwner (tilesState (boardState updatedGameState)) `shouldBe` [Nothing, Nothing, Nothing]

    context "when there are no tiles on the board" $ do
      it "does not change the ownership status of any tile" $ do
        let initialGameState = GameState (PlayersState [PlayerState 1 100 0 []] 1) (BoardState 0 []) (TurnState 1 "" False (0, 0) False False)
            updatedGameState = clearOwnershipFromTiles initialGameState
        map tileOwner (tilesState (boardState updatedGameState)) `shouldBe` []

-}