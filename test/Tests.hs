{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main (main) where

{-
This module contains unit tests for the game logic

This is a good tutorial for setting up HUnit with cabal:
https://putridparrot.com/blog/unit-testing-haskel-code-with-hunit/
-}

import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts(errors, failures),
      Test(TestCase),
      Testable(test) )
import System.Exit
import Data
import Game


-- wonGame tests

wonGameWithNoGuesses = TestCase do
    let game = initializeGame "CRAZY"

    let result = wonGame game
    assertEqual "wonGameWithNoGuesses" False result

wonGameWithIncorrectGuess = TestCase do
    let guesses = [ createGuessFromAnswer  "MERRY" Incorrect,
                    createGuessFromAnswer  "BARON" PartlyCorrect]
    let game = initializeGameWithGuesses "REALM" guesses

    let result = wonGame game
    assertEqual "wonGameWithIncorrectGuess" False result

wonGameWithCorrectGuess = TestCase do
    let guesses = [ createGuessFromAnswer  "MERRY" Incorrect,
                    createGuessFromAnswer  "BARON" PartlyCorrect,
                    createGuessFromAnswer  "REALM" Correct]
    let game = initializeGameWithGuesses "REALM" guesses

    let result = wonGame game
    assertEqual "wonGameWithCorrectGuess" True result


-- winningGuess tests

winningGuessWithIncorrectGuess = TestCase do
    let guess = createGuessFromAnswer  "MERRY" Incorrect

    let result = winningGuess "REALM" guess
    assertEqual "winningGuessWithIncorrectGuess" False result

winningGuessWithEmptyGuess = TestCase do
    let guess = createGuessFromAnswer  "" Incorrect

    let result = winningGuess "REALM" guess
    assertEqual "winningGuessWithEmptyGuess" False result

winningGuessWithCorrectGuess = TestCase do
    let guess = createGuessFromAnswer  "REALM" Correct

    let result = winningGuess "REALM" guess
    assertEqual "winningGuessWithCorrectGuess" True result

-- toWord tests
toWordEmpty = TestCase do
    let guess = createGuessFromAnswer "" None

    let result = toWord guess
    assertEqual "toWordEmpty" "" result 

toWordSuccess = TestCase do
    let guess = createGuessFromAnswer "TESTS" None

    let result = toWord guess
    assertEqual "toWordEmpty" "TESTS" result 

-- submittedGuesses tests
submittedGuessesNone = TestCase do
    let game = initializeGame "TESTS"

    let result = submittedGuesses game
    assertEqual "submittedGuessesNone" [] result
    assertEqual "submittedGuessCount 0" (submittedGuessCount game) 0

submittedGuessesMultiple = TestCase do
    let submitted1 = createGuessFromAnswer "WORDL" Incorrect
    let submitted2 = createGuessFromAnswer "HURDL" PartlyCorrect
    let submitted3 = createGuessFromAnswer "TURTL" Correct
    let guesses = [ submitted1,
                    submitted2,
                    submitted3,
                    createGuessFromAnswer "BURTL" None,
                    createGuessFromAnswer "AB" None,
                    createGuessFromAnswer "" None]
    let game = initializeGameWithGuesses "WORDL" guesses

    let result = submittedGuesses game
    assertEqual "submittedGuessesMultiple" [submitted1, submitted2, submitted3] result
    assertEqual "submittedGuessCount 3" (submittedGuessCount game) 3

-- Tests for currentGuess :: Game -> Guess
currentGuessSingle = TestCase do
    let guess = createGuessFromAnswer "BURTL" None
    let guesses = [ guess ]
    let game = initializeGameWithGuesses "WORDL" guesses
    
    let result = currentGuess game
    assertEqual "currentGuessFirst" guess result
    assertEqual "currentGuessIndex 0" (currentGuessIndex game) 0

currentGuessMultiple = TestCase do
    let lastGuess = createGuessFromAnswer "HURTS" None
    let guesses = [ createGuessFromAnswer "AB" Incorrect,
                    createGuessFromAnswer "" PartlyCorrect, 
                    lastGuess ]
    let game = initializeGameWithGuesses "WORDL" guesses
    
    let result = currentGuess game
    assertEqual "currentGuessLast" lastGuess result
    assertEqual "currentGuessIndex 2" (currentGuessIndex game) 2

processEnterKeySuccess = TestCase do
    let gs = [ createGuessFromAnswer "REALM" None]
    let game = initializeGameWithGuesses "WORDL" gs
    let config = initializeConfig [] []

    let result = processEnterKey game config
    assertEqual "processEnterKeySuccess currentGuessIsSubmitted" True (currentGuessIsSubmitted game)
    assertEqual "processEnterKeySuccess guessCount" 2 (length $ guesses game)

processEnterKeyTooShort = TestCase do
    let gs = [ createGuessFromAnswer "REA" None]
    let game = initializeGameWithGuesses "WORDL" gs
    let config = initializeConfig [] []

    let result = processEnterKey game config
    assertEqual "processEnterKeyTooShort currentGuessIsSubmitted" False (currentGuessIsSubmitted game)
    assertEqual "processEnterKeyTooShort guessCount" 2 (length $ guesses game)
    assertEqual "" game result

{-  **** ENTRY POINT **** -}
main :: IO ()
main = do
    counts <- runTestTT (test [ wonGameWithNoGuesses, wonGameWithIncorrectGuess, wonGameWithCorrectGuess, 
                                winningGuessWithIncorrectGuess, winningGuessWithEmptyGuess, winningGuessWithCorrectGuess,
                                toWordEmpty, toWordSuccess, 
                                submittedGuessesNone, submittedGuessesMultiple,
                                currentGuessSingle, currentGuessMultiple,
                                processEnterKeySuccess, processEnterKeyTooShort ])

    if errors counts + failures counts == 0 
        then exitSuccess 
        else exitFailure


-- Utility functions for creating test data
createGuessFromAnswer :: String -> Result -> Guess
createGuessFromAnswer xs r = 
    map (\c -> (c,r)) xs

initializeGameWithGuesses :: Answer -> Guesses -> Game
initializeGameWithGuesses a gs = game { guesses = gs }
    where game = initializeGame a