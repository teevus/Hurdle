module Data (
    Config (..),
    Game (..),
    Hints (..),
    Hint (..),
    Answer (..),
    Guess (..),
    GuessChar (..),
    Guesses (..)
) where

import Control.Monad.State
import Control.Monad.Reader

type Answer = String
type GuessChar = (Char, Result)
type Guess = [GuessChar]
type Guesses = [Guess]

type Hint = String
type Hints = [Hint]

data Result = Correct | PartlyCorrect | Incorrect | None

-- Indicates a position on the screen
type Point = (Int, Int)

data Config = Config {
    allowedGuessCount :: Int
}

data Game = Game {
    guesses :: Guesses
}