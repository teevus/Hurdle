module Data (
    Config (..),
    Game (..),
    Hints (..),
    Hint (..),
    Answer (..),
    Guess (..),
    GuessChar (..),
    Guesses (..),
    Point (..),
    Result (..)
) where
    -- MO TODO: Review exports once its all working

import Control.Monad.State
import Control.Monad.Reader
import System.Console.ANSI

type Answer = String
type GuessChar = (Char, Result)
type Guess = [GuessChar]
type Guesses = [Guess] 

type Hint = String
type Hints = [Hint]

-- The Result of a Guess.  None indicates that the guess has not been evaluated yet 
data Result = Correct | PartlyCorrect | Incorrect | None 
    deriving (Eq, Show)

-- Indicates a position on the screen
type Point = (Int, Int)

data Config = Config {
    maxGuesses         :: Int,      -- Number of guesses allowed
    hintCount          :: Int,      -- Number of hints to display
    foregroundColor    :: Color,    -- foreground color for all guess results
    backgroundColor    :: Color,    -- background color for empty or unsubmitted guesses
    correctColor       :: Color,    -- the background color for correctly guessed letters
    partlyCorrectColor :: Color,    -- the background color for partly correct letters
    incorrectColor     :: Color,    -- the background color for incorrect letters
    validGuesses       :: [String], -- The set of words that will be considered valid guesses
    possibleAnswers    :: [Answer], -- The set of words that could be possible answers (a subset of validGuesses)
    showDebug          :: Bool      -- When true, debug values will be displayed during rendering
}

instance Show Config where
    show c = "Config {maxGuesses = " ++ show (maxGuesses c) ++
             "        hintCount = " ++ show (hintCount c) ++
             "        foregroundColor = " ++ show (foregroundColor c) ++
             "        backgroundColor = " ++ show (backgroundColor c) ++
             "        correctColor = " ++ show (correctColor c) ++
             "        partlyCorrectColor = " ++ show (partlyCorrectColor c) ++
             "        incorrectColor = " ++ show (incorrectColor c) ++
             "        validGuesses count = " ++ show (length $ validGuesses c) ++
             "        possibleAnswers count = " ++ show (length $ possibleAnswers c) ++
             "        showDebug = " ++ show (showDebug c)

data Game = Game {
    answer              :: Answer,     -- The correct answer
    guesses             :: Guesses,    -- The guesses the player has made
    hints               :: Hints,      -- Hints to display
    showInstructions    :: Bool,       -- Whether to display instructions on screen can be toggled on/off by the player
    showHints           :: Bool,       -- Whether to display hints on screen can be toggled on/off by the player
    userQuit            :: Bool,       -- Indicates that the user has pressed escape to exit the game
    showInvalidMessage  :: Bool        -- Indicates that the user has attempted to submit the current guess but its not an acceptible word
} deriving (Eq, Show)
