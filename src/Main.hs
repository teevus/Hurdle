module Main where

import System.Console.ANSI
import System.Random

type Answer = String
type Guess = String
type Guesses = [String]

-- Pure functions

selectRandomItem :: StdGen -> [a] -> a
selectRandomItem gen xs = xs !! rand 
    where (rand, _) = randomR (0, length xs - 1) gen 

    

-- IO/ Impure functions

loadPossibleAnswers :: IO [Answer]
loadPossibleAnswers = do
    return ["TESTS", "TANKS", "TUBBY", "TOOLS"]  -- TODO: Load from file

loadAllowedGuesses :: IO Guesses
loadAllowedGuesses = do
    return ["TESTS", "TANKS", "TUBBY", "REALM", "RESTS"] -- TODO: Load from file

selectRandomAnswer :: [Answer] -> IO Answer
selectRandomAnswer xs = do
    gen <- getStdGen
    return $ selectRandomItem gen xs

putStrWithColor :: String -> Color -> Color -> IO ()
putStrWithColor str fg bg = do
    setSGR [SetColor Foreground Vivid fg]
    setSGR [SetColor Background Vivid bg]
    putStr str
    setSGR [Reset]

writeInstructions :: IO ()
writeInstructions = do
    putStrLn  "--------------------------------------------------------------------------------------------------"
    putStrLn  "HURDLE:  Haskell version of WORDLE, the addictive game by Josh Wardle."
    putStrLn  "Available at https://www.nytimes.com/games/wordle"
    putStrLn  "--------------------------------------------------------------------------------------------------"
    putStrLn  ""
    putStrLn  "Guess the WORD in six tries"
    putStrLn  "Each guess must be a valid five-letter word. Hit the enter button to submit."
    putStrLn  "After each guess, the color of the tiles will change to show how close your guess was to the word."
    putStrLn  ""
    putStrLn  "Examples:"
    putStrLn  ""
    putStrWithColor "W" Black Green
    putStr    "EARY     "
    putStrLn  "The letter W is in the word and is in the correct spot"
    putStrLn  ""
    putStr    "P"
    putStrWithColor "I" Black Yellow
    putStr    "LLS     "
    putStrLn  "The letter I is in the word but in the wrong spot"
    putStrLn  ""
    putStr    "VAG"
    putStrWithColor "U" Black White
    putStr    "E     "
    putStrLn  "The letter U is not in the word in any spot"
    putStrLn  ""

-- MAIN
main :: IO ()
main = do
    possibleAnswers <- loadPossibleAnswers
    answer <- selectRandomAnswer possibleAnswers 
    allowedGuesses <- loadAllowedGuesses

    writeInstructions

    putStrLn answer
