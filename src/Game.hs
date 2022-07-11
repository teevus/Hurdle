{-# LANGUAGE ScopedTypeVariables #-}

{-
This module contains functions that operate on the Game and related data types such as Guess etc.
This module is for (non-IO related) game logic, and should only contain pure functions (no IO monad).
Rendering logic is in Render.hs, not in this module
-}
module Game (
    submittedGuesses,
    wonGame,
    gameOver,
    toWord,
    guessCount,
    currentGuess,
    addLetter,
    removeLetter,
    evaluateGuesses,
    startNextRow
) where

import Data
import Utils 

-- Returns the guesses that have been submitted
submittedGuesses :: Game -> [Guess]
submittedGuesses g = filter isSubmitted (guesses g)

-- Retrieves the number of guesses that have been submitted
guessCount :: Game -> Int
guessCount g = length $ submittedGuesses g

currentGuess :: Game -> Guess
currentGuess g = last $ guesses g

-- Determines whether a particular guess has been submitted
isSubmitted :: Guess -> Bool
isSubmitted g = (length g) == 5 && all (\(_,r) -> r /= None) g

-- Determines whether the game is finished (i.e. maximum number of guesses have been submitted)
-- This will return true if the game is finished regardless of whether the game was won or lost
gameOver :: Game -> Config -> Bool
gameOver g c = (guessCount g) >= (maxGuesses c) || any (winningGuess (answer g)) (guesses g)

-- Determines whether the player has won the game
wonGame :: Game -> Bool
wonGame g = any (winningGuess (answer g)) (guesses g)

-- Determine whether the specified guess is a winning guess
winningGuess :: Answer -> Guess -> Bool
winningGuess a g = toWord g == a

-- Converts a guess (which is a list of tuples) to the actual word that was guessed (String)
toWord :: Guess -> String
toWord g = map fst g

-- Adds a letter to the current guess and returns a list containing the guesses
addLetter :: Game -> Char -> Guesses
addLetter game c = replaceElem gs (guessCount game) modifiedGuess
    where gs = guesses game
          currGuess = currentGuess game
          modifiedGuess = addLetterToGuess currGuess c

-- Adds a letter to the specified guess (unless its already full)
addLetterToGuess :: Guess -> Char -> Guess
addLetterToGuess g c = if length g < 5 then g ++ [(c, None)] else g
-- Note: adding to the end of a list in Haskell can be expensive, but in this case our list will have maximum length of 6

-- Returns a list of guesses, which contains the existing guesses, but with the last letter removed
-- Note: the last letter can only be removed if the guess has not been submitted yet (i.e. Result will be None)
removeLetter :: Game -> Guesses
removeLetter game = replaceElem gs (guessCount game) modifiedGuess
    where gs = guesses game
          currGuess = currentGuess game
          modifiedGuess = removeLetterFromGuess currGuess
-- MO TODO: There is some similarity between addLetter and removeLetter, so I could refactor this

removeLetterFromGuess :: Guess -> Guess  -- Note: Guess is simply [GuessChar]
removeLetterFromGuess [] = []
removeLetterFromGuess gc = init gc

-- Evaluates all guesses and returns a list of the guesses with the Result value populated
evaluateGuesses :: Game -> Guesses
evaluateGuesses g = map (evaluateGuess (answer g)) (guesses g)

-- Evaluates the specified guess, returning a Guess type which is a list of GuessChar values with the Result value populated
evaluateGuess :: Answer -> Guess -> Guess
-- evaluateGuess :: Answer -> [GuessChar] -> [GuessChar]
evaluateGuess a gcs = map (\(i,gc) -> evaluateGuessChar a i gc) (zip [0..] gcs)

evaluateGuessChar :: Answer -> Int -> GuessChar -> GuessChar
evaluateGuessChar a index (c,_)
    | a !! index == c  = (c,Correct)
    | c `elem` a       = (c,PartlyCorrect)
    | otherwise        = (c,Incorrect)


startNextRow :: Game -> Guesses
startNextRow g = guesses g ++ [[]]
