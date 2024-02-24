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

-- function for 