{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
    ( execState, MonadState(put, get), State, StateT(runStateT) )
import Data.List
import Data.Char
import System.Console.ANSI
import System.IO
import System.Random

import Data
import Render
import Utils
import Game

-- IO/ Impure functions

-- Randomly chooses some words from the available words that have matching letter positions
-- For example, we could randomly pick 5 words out of the top 100 matching words
-- This is so that we have some variability for the player, but also prioritize words with more commonly occuring letters that have not yet been eliminated

loadPossibleAnswers :: IO [Answer]
loadPossibleAnswers = do
    contents <- readFile "data/solutions.txt"
    gen <- getStdGen
    return $ shuffle gen $ parseWords contents

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
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    -- Need some basic config to render the loading screen with the instructions
    let tempConfig = initializeConfig [] []
    renderLoading tempConfig True

    gen <- getStdGen
    possibleAnswers <- loadPossibleAnswers
    answer <- selectRandomAnswer possibleAnswers
    validGuesses <- loadValidGuesses

    let config = initializeConfig validGuesses possibleAnswers
    let game = initializeGame answer

    renderLoading config False
    getLine

    runStateT (runReaderT playGameM config) game

-- This is the main game loop
playGameM :: ReaderT Config (StateT Game IO) ()
playGameM = do
    renderGameM
    processNextInputM

    game <- get
    config <- ask

    -- User pressed escape to quit - prompt them if they really want to
    if userQuit game then do
        
        liftIO $ putStrLn "Are you sure you want to QUIT? (Y/N)"
        liftIO $ hSetEcho stdin False
        c <- liftIO getHiddenChar
        liftIO $ hSetEcho stdin True
        when (toUpper c /= 'Y') $ do
            put $ game { userQuit = False}
            playGameM

    -- Check if we've reached game over state, and prompt the user whether to play another game if we have
    else if gameOver game config then do
        
        renderGameM
        playAgain <- liftIO $ renderGameOver game
        when playAgain $ do
            -- Reset the game state
            answer <- liftIO $ selectRandomAnswer (possibleAnswers config)
            put $ initializeGame answer
            playGameM

    else do -- Game is still in process

        playGameM


-- This is the main rendering function that gets called each time the game state has changed
renderGameM :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
renderGameM = do
    game <- get
    config <- ask
    liftIO $ renderGame game config

-- Accepts user input, and updates the Game state as required
processNextInputM :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
processNextInputM = do
    game <- get
    config <- ask

    liftIO $ hSetEcho stdin False
    c <- liftIO getHiddenChar
    when (showDebug config) $ liftIO $ putStrLn (show $ fromEnum c)
    liftIO $ hSetEcho stdin True

    let (modified, modifiedGame) = processUserInput c game config
    if modified then
        put modifiedGame
    else
        processNextInputM

