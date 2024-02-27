module TestSpec where

import Test.Hspec
import System.Directory (doesFileExist, removeFile)
import Control.Exception (evaluate)
import Data.Binary (encode, decode, encodeFile, decodeFile)
import qualified Data.ByteString.Lazy as BS

-- Test Cases for saveGameState
-- | Save GameState to a file 
saveGameState :: FilePath -> GameState -> IO()
saveGameState filePath gs = encodeFile filePath gs

import Test.Hspec
import System.Directory (doesFileExist, removeFile)
import Control.Exception (evaluate)

-- Import your functions and types
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as BS

-- Import the types you want to test
import Monopoly (GameState(..))

main :: IO ()
main = hspec $ do
  describe "saveGameState" $ do
    it "should save the game state to a file" $ do
      let testData = GameState playersStateSample boardStateSample turnStateSample -- Sample game state data
          testFilePath = "test_data.bin"  -- Test file path
      -- Save the game state
      saveGameState testFilePath testData
      -- Check if the file exists
      fileExists <- doesFileExist testFilePath
      fileExists `shouldBe` True

    it "should save and load the same game state" $ do
      let testData = GameState playersStateSample boardStateSample turnStateSample -- Sample game state data
          testFilePath = "test_data.bin"  -- Test file path
      -- Save the game state
      saveGameState testFilePath testData
      -- Load the game state
      loadedData <- loadGameState testFilePath
      -- Compare the loaded data with the original data
      loadedData `shouldBe` testData

-- Sample data for testing
playersStateSample :: PlayersState
playersStateSample = (PlayersState [01,02] 01)

boardStateSample :: BoardState
boardStateSample = (BoardState 0 00 0 0)

turnStateSample :: TurnState
turnStateSample = (TurnState 2 "" True (2,3))

-- Clean up after tests
  after_ (removeFile "test_data.bin") $ return ()

-- Test Cases for loadGameState
-- | Load GameState from a file
loadGameState :: FilePath -> IO GameState
loadGameState filePath = decodeFile filePath

import Test.Hspec
import System.Directory (doesFileExist, removeFile)
import Control.Exception (evaluate)

-- Import your functions and types
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as BS

-- Import the types you want to test
import YourModule (GameState(..))

main :: IO ()
main = hspec $ do
  describe "loadGameState" $ do
    it "should load the game state from a file" $ do
      let testData = GameState playersStateSample boardStateSample turnStateSample -- Sample game state data
          testFilePath = "test_data.bin"  -- Test file path
      -- Save the game state
      BS.writeFile testFilePath (encode testData)
      -- Load the game state
      loadedData <- loadGameState testFilePath
      -- Compare the loaded data with the original data
      loadedData `shouldBe` testData

    it "should fail to load a non-existent file" $ do
      let nonExistentFilePath = "non_existent_file.bin"  -- Non-existent file path
      -- Try loading the game state from a non-existent file
      loadResult <- loadGameState nonExistentFilePath
      -- Ensure that an exception is thrown
      loadResult `shouldThrow` anyIOException

-- Sample data for testing
playersStateSample :: PlayersState
playersStateSample = (PlayersState [01,02] 01)

boardStateSample :: BoardState
boardStateSample = (BoardState 0 00 0 0)

turnStateSample :: TurnState
turnStateSample = (TurnState 2 "" True (2,3))

-- Clean up after tests
after_ (removeFile "test_data.bin") $ return ()