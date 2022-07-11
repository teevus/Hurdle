module Main where

import System.Random
import Control.Monad.Reader
import Control.Monad.State

import Data
import Render


-- Pure functions
-- Selects an item at random from the specified list
selectRandomItem :: StdGen -> [a] -> a
selectRandomItem gen xs = xs !! rand 
    where (rand, _) = randomR (0, length xs - 1) gen 


-- IO/ Impure functions

loadPossibleAnswers :: IO [Answer]
loadPossibleAnswers = do
    return ["TESTS", "TANKS", "TUBBY", "TOOLS"]  -- TODO: Load from file

loadAllowedGuesses :: IO [Answer]
loadAllowedGuesses = do
    return ["TESTS", "TANKS", "TUBBY", "REALM", "RESTS"] -- TODO: Load from file

selectRandomAnswer :: [Answer] -> IO Answer
selectRandomAnswer xs = do
    gen <- getStdGen
    return $ selectRandomItem gen xs

initializeConfig :: Config
initializeConfig = Config { guessCount = 6 }

initializeGame :: Game
initializeGame = Game { guesses = [] }

-- MAIN
main :: IO ()
main = do
    possibleAnswers <- loadPossibleAnswers
    answer <- selectRandomAnswer possibleAnswers 
    allowedGuesses <- loadAllowedGuesses
    
    let config = initializeConfig
    let game = initializeGame

    renderInstructions config

    renderBoard game config

    -- MO TODO: Accept user input and process main loop 
    putStrLn $ "ANSWER: " ++ answer
