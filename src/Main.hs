module Main where

import System.Random
import Control.Monad.Reader
import Control.Monad.State
import System.Console.ANSI

import Data
import Render


-- Pure functions
-- Selects an item at random from the specified list
selectRandomItem :: StdGen -> [a] -> a
selectRandomItem gen xs = xs !! rand 
    where (rand, _) = randomR (0, length xs - 1) gen 


-- Randomly chooses some words from the available words that have matching letter positions
-- MO TODO: Ideally this should prioritize words that have commonly occuring letters, that have not yet been eliminated 



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

initializeConfig :: Answer -> [Answer] -> Config
initializeConfig a aa = Config { guessCount = 6, 
                                 hintCount = 5,
                                 backgroundColor = Black,
                                 correctColor = Green,
                                 partlyCorrectColor = Yellow,
                                 incorrectColor = White,
                                 answer = a,
                                 allowedAnswers = aa }

initializeGame :: Game
initializeGame = Game { guesses = [], 
                        showInstructions = True, 
                        showHints = True,
                        hints = [] }

-- MAIN
main :: IO ()
main = do
    possibleAnswers <- loadPossibleAnswers
    answer <- selectRandomAnswer possibleAnswers 
    allowedGuesses <- loadAllowedGuesses

    let config = initializeConfig answer allowedGuesses
    let game = initializeGame

    renderGame game config

    -- MO TODO: Accept user input and process main loop 
    putStrLn $ "ANSWER: " ++ answer
