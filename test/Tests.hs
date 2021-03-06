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
import System.IO
import System.Random
import Data.List
-- import System.Exit
import Debug.Trace
import Data
import Game
import Utils


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

wonGameNotSubmitted = TestCase do
    let guesses = [ createGuessFromAnswer  "REALM" None]
    let game = initializeGameWithGuesses "REALM" guesses

    let result = wonGame game
    assertEqual "wonGameNotSubmitted" False result

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
    let g = createGuessFromAnswer "REALM" None
    let game = initializeGameWithGuesses "WORDL" [g]
    let config = initializeConfig ["REALM","WORDL"] ["REALM","WORDL"]

    assertEqual "PRE processEnterKeySuccess submittedCount" 0 (length $ submittedGuesses game)
    assertEqual "PRE processEnterKeySuccess toWord" True (toWord g == "REALM")
    assertEqual "PRE processEnterKeySuccess currentGuessIsValid" True (currentGuessIsValid config game)

    traceIO "-------------"
    traceIO $ show game

    let result = processEnterKey game config

    traceIO "-------------"
    traceIO $ show result

    assertEqual "processEnterKeySuccess currentGuessIsSubmitted" False (currentGuessIsSubmitted result)
    assertEqual "processEnterKeySuccess currentGuessIsValid" False (currentGuessIsValid config result)
    assertEqual "processEnterKeySuccess guessCount" 2 (length $ guesses result)
    assertEqual "processEnterKeySuccess submittedCount" 1 (length $ submittedGuesses result)

processEnterKeyTooShort = TestCase do
    let gs = [ createGuessFromAnswer "REA" None]
    let game = initializeGameWithGuesses "WORDL" gs
    let config = initializeConfig [] []

    let result = processEnterKey game config
    assertEqual "processEnterKeyTooShort currentGuessIsSubmitted" False (currentGuessIsSubmitted result)
    assertEqual "processEnterKeyTooShort currentGuessIsValid" False (currentGuessIsValid config result)
    assertEqual "processEnterKeyTooShort guessCount" 1 (length $ guesses result)
    assertEqual "" game result

currentGuessIsFinishedTrue = TestCase do
    let gs = [ createGuessFromAnswer "AAAAA" None]
    let game = initializeGameWithGuesses "WORDL" gs

    let result = currentGuessIsFinished game
    assertEqual "currentGuessIsFinishedTrue" True result

currentGuessIsFinishedFalse = TestCase do
    let gs = [ createGuessFromAnswer "AAAA" None]
    let game = initializeGameWithGuesses "WORDL" gs

    let result = currentGuessIsFinished game
    assertEqual "currentGuessIsFinishedFalse" False result

guessIsValidTrue = TestCase do
    let config = initializeConfig ["TESTS", "TORTS"] ["TARTS", "TORTS"]
    let guess = createGuessFromAnswer "TORTS" None
    let result = guessIsValid config guess
    assertEqual "guessIsValidTrue" True result

guessIsValidFalse = TestCase do
    let config = initializeConfig ["TESTS", "TORTS"] ["TESTS", "TORTS"]
    let guess = createGuessFromAnswer "TOAST" None
    let result = guessIsValid config guess
    assertEqual "guessIsValidFalse" False result

getHintsOneGuess = TestCase do
    let possibleAnswers = ["CIGAR", "REBUT", "SISSY", "HUMPH", "AWAKE", "BLUSH", "FOCAL", "EVADE", "NAVAL"]
    let game = initializeGame "AWAKE"
    let guesses = [[('C', Correct),('R',PartlyCorrect),('Z',Incorrect),('E', Incorrect), ('G', Correct)]]
    let hints = getHints 5 guesses possibleAnswers
    assertEqual "getHintsOneGuess count" 1 (length hints)
    assertEqual "getHintsOneGuess result" "CIGAR" (head hints)

shuffleTen = TestCase do
    let values = [1..10]
    gen <- getStdGen

    let result = shuffle gen values
    assertEqual "shuffleTen length" 10 (length result) 
    assertEqual "shuffleTen eq" True (values == sort result)
    traceIO $ show result

shuffleEmpty = TestCase do
    gen <- getStdGen
    assertEqual "shuffleEmpty" True (null (shuffle gen []))

selectRandomItemsFive = TestCase do
    gen <- getStdGen
    let result = selectRandomItems gen 5 [1..10]
    assertEqual "selectRandomItemsFive" 5 (length result)
    traceIO $ show result

selectRandomItemsEmpty = TestCase do
    gen <- getStdGen 
    
    let result = selectRandomItems gen 0 [1..10]
    assertEqual "selectRandomItemsEmpty" 0 (length result)

{-  **** ENTRY POINT **** -}
main :: IO ()
main = do
    counts <- runTestTT (test [ wonGameWithNoGuesses, wonGameWithIncorrectGuess, wonGameWithCorrectGuess, wonGameNotSubmitted,
                                winningGuessWithIncorrectGuess, winningGuessWithEmptyGuess, winningGuessWithCorrectGuess,
                                toWordEmpty, toWordSuccess,
                                submittedGuessesNone, submittedGuessesMultiple,
                                currentGuessSingle, currentGuessMultiple,
                                processEnterKeySuccess, processEnterKeyTooShort,
                                currentGuessIsFinishedTrue, currentGuessIsFinishedFalse,
                                guessIsValidTrue, guessIsValidFalse,
                                getHintsOneGuess,
                                shuffleTen, shuffleEmpty,
                                selectRandomItemsEmpty, selectRandomItemsFive]) 

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

