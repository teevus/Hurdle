module Main where

import System.Random
import Control.Monad.Reader
import Control.Monad.State
import System.Console.ANSI
import Data.List

import Data
import Render
import Utils


-- Pure functions

-- Determines whether the specified string matches the guessed character
matchesGuess :: String -> GuessChar -> Bool
-- matchesGuess _ (_, None) = False -- I'd prefer an error since this shouldnt ever be passed to this function
matchesGuess s (c, Incorrect) = c `notElem` s      -- notElem returns True if the element is not contained within the list (from Data.List)
matchesGuess s (c, PartlyCorrect) = c `elem` s     -- elem return True if the element is contained within the list (from Data.List)
matchesGuess s (c, Correct) = c `elem` s    -- MO TODO: The index of Correct matches needs to be passed in

-- Determines whether the specified string matches the list of guessed characters
matchesGuesses :: [GuessChar] -> String -> Bool
matchesGuesses [] s = True
matchesGuesses xs s = all (matchesGuess s) xs

-- select n words from the list of possible answers that contain the known letters in the correct place
-- MO TODO: Ensure this is lazy evaluating so we only retrieve the first n results
getHints :: Int -> Guesses -> [Answer] -> [String]
getHints n gs pa = take n (filter (matchesGuesses g) pa)
                   where g = knownResults gs

-- Collates what we know from the results of the guesses thus far into a structure thats more usable
knownResults :: Guesses -> [GuessChar]
knownResults [] = []
knownResults gs = filter (\(_,r) -> r /= None) (nub $ concat gs)


-- IO/ Impure functions

-- Randomly chooses some words from the available words that have matching letter positions
-- For example, we could randomly pick 5 words out of the top 100 matching words
-- This is so that we have some variability for the player, but also prioritize words with more commonly occuring letters that have not yet been eliminated

-- MO TODO: prioritize words that have commonly occuring letters, that have not yet been eliminated 
randomHints :: Int -> Int -> Guesses -> [Answer] -> IO Hints
randomHints n nmax g a = do
    gen <- getStdGen
    return $ selectRandomItems gen n (getHints nmax g a)

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
                                 backgroundColor = Blue,
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
