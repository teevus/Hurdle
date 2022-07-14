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
        
        if wonGame game then
            put $ game { helpText = "CONGRATULATIONS: You won in " ++ show (submittedGuessCount game) ++ " attempts!" }
        else
            put $ game { helpText = "BAD LUCK: You lost!" }

        -- game <- get    -- MO TODO: Is this necessary and does it need to be different to game?
        renderGameM
        playAgain <- liftIO $ renderGameOver game
        when playAgain $ do
            -- Reset the game state
            answer <- liftIO $ selectRandomAnswer (possibleAnswers config)
            put $ initializeGame answer
            playGameM

    else do -- Game is still in process
        if currentGuessIsFinished game && not (currentGuessIsSubmitted game) then
            put $ game { helpText = "Press ENTER to Submit (or press SPACE for Hints)" }
        else
            put $ game { helpText = "Enter a 5 letter word (or press SPACE to show Hints)" }    -- MO TODO: Move this helpText stuff into a separate function
        
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

