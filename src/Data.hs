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

data Result = Correct | PartlyCorrect | Incorrect | None 
    deriving (Eq, Show)

-- Indicates a position on the screen
type Point = (Int, Int)

data Config = Config {
    guessCount         :: Int,      -- Number of guesses allowed
    hintCount          :: Int,      -- Number of hints to display
    backgroundColor    :: Color,
    correctColor       :: Color,
    partlyCorrectColor :: Color,
    incorrectColor     :: Color,
    answer             :: Answer,
    allowedAnswers     :: [Answer]  --  The set of words that will be considered valid guesses
} deriving Show

data Game = Game {
    guesses          :: Guesses,    
    hints            :: Hints,     -- Hints to display
    showInstructions :: Bool,      -- Whether to display instructions on screen can be toggled on/off by the player
    showHints        :: Bool       -- Whether to display hints on screen can be toggled on/off by the player
} deriving Show