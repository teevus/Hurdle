{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Control.Concurrent
import Control.Monad.Except -- MO TODO: Not sure if I need this, but its in the snake project
import Control.Monad.Reader
import Control.Monad.State
    ( execState, MonadState(put, get), State, StateT(runStateT) )
import Data.List
-- import Data.List.Split
import Data.Char
import System.Console.ANSI
import System.IO
import System.Random

import Data
import Render
import Utils
import Game


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
    gen <- newStdGen
    return $ selectRandomItems gen n (getHints nmax g a)

loadPossibleAnswers :: IO [Answer]
loadPossibleAnswers = do
    contents <- readFile "data/solutions.txt"
    return $ parseWords contents

loadValidGuesses :: IO [Answer]
loadValidGuesses = do
    contents <- readFile "data/allwords.txt"
    return $ parseWords contents

selectRandomAnswer :: [Answer] -> IO Answer
selectRandomAnswer xs = do
    gen <- newStdGen
    return $ selectRandomItem gen xs

-- MAIN
main :: IO ((), Game)
main = do
    -- MO TODO: This is not working properly in windows terminal
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    -- Need some basic config to render the loading screen with the instructions
    let tempConfig = initializeConfig [] []
    renderLoading tempConfig True

    possibleAnswers <- loadPossibleAnswers
    answer <- selectRandomAnswer possibleAnswers
    validGuesses <- loadValidGuesses

    let config = initializeConfig validGuesses possibleAnswers
    let game = initializeGame answer

    renderLoading config False
    getLine

    runStateT (runReaderT playGame config) game

-- This is the main game loop
playGame :: ReaderT Config (StateT Game IO) ()
playGame = do
    renderGameM
    continue <- processNextM
    when continue
        playGame

-- This is the main rendering function that gets called each time the game state has changed
renderGameM :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
renderGameM = do
    game <- get
    config <- ask
    liftIO $ renderGame game config

-- Accepts user input, and updates the Game state as required
-- Returns True:  keep playing, 
--         False: The application should exit
processNextM :: (MonadIO m, MonadReader Config m, MonadState Game m) => m Bool
processNextM = do
    game <- get
    config <- ask
    let currGuess = currentGuess game
    let guessIsFinished = length currGuess == 5
    let currGuessIsSubmitted = isSubmitted currGuess

    if guessIsFinished && not currGuessIsSubmitted then do
        liftIO getLine
        evaluateGuessesM
        unless (gameOver game config) startNextRowM
    else do
        liftIO $ hSetEcho stdin False
        c <- liftIO getChar
        liftIO $ hSetEcho stdin True

        -- MO TODO: Accept backspace character for delete
        if c == ' ' then toggleHintsM
        else if c == '!' then toggleInstructionsM
        else if c `elem` ['a'..'z'] ++ ['A'..'Z'] then addLetterM (toUpper c)
        else when (c == '-') removeLetterM -- still awaiting further user input

    -- Check if we've reached game over state
    if gameOver game config then do
        renderGameM
        playAgain <- liftIO $ renderGameOver game
        if playAgain then do
            -- Reset the game state
            answer <- liftIO $ selectRandomAnswer (possibleAnswers config)
            let newGame = initializeGame answer
            put newGame
            return True
        else
            return False
    else
        return True

processUserInputM :: (MonadReader Config m, MonadState Game m) => Char -> m ()
processUserInputM c = do
    game <- get
    config <- ask
    let modifiedGame = processUserInput c game
    put modifiedGame

toggleHintsM :: MonadState Game m => m ()
toggleHintsM = do
    game <- get
    let currentValue = showHints game
    put (game { showHints = not currentValue })

toggleInstructionsM :: MonadState Game m => m ()
toggleInstructionsM = do
    game <- get
    let currentValue = showInstructions game
    put (game { showInstructions = not currentValue })

-- MO TODO: Only run the code to update the MonadState in a single place?
addLetterM :: MonadState Game m => Char -> m ()
addLetterM c = do
    game <- get
    let modifiedGame = addLetter game c
    put modifiedGame

removeLetterM :: MonadState Game m => m ()
removeLetterM = do
    game <- get
    let modifiedGame = removeLetter game
    put modifiedGame

evaluateGuessesM :: (MonadReader Config m, MonadState Game m)=> m ()
evaluateGuessesM = do
    game <- get
    let modifiedGame = evaluateGuesses game
    put modifiedGame

startNextRowM :: (MonadReader Config m, MonadState Game m)=> m ()
startNextRowM = do
    game <- get
    cfg <- ask
    let modifiedGame = startNextRow game cfg
    put modifiedGame
