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
    gen <- getStdGen
    return $ selectRandomItems gen n (getHints nmax g a)

loadPossibleAnswers :: IO [Answer]
loadPossibleAnswers = do
    contents <- readFile "data/solutions.txt"
    let answers = map (map toUpper) (words contents)
    return answers
-- return ["TESTS", "TANKS", "TUBBY", "TOOLS"]  --For testing

loadValidGuesses :: IO [Answer]
loadValidGuesses = do
    contents <- readFile "data/allwords.txt"
    let answers = map (map toUpper) (words contents)
    return answers
-- return $ ["REALM", "RESTS", "WEIRD"]  ++ possibleAnswers --For testing

selectRandomAnswer :: [Answer] -> IO Answer
selectRandomAnswer xs = do
    gen <- getStdGen
    return $ selectRandomItem gen xs

initializeConfig :: [Answer] -> [Answer] -> Config
initializeConfig vg pa = Config { maxGuesses = 6,
                                  hintCount = 5,
                                  foregroundColor = Black,
                                  backgroundColor = White,
                                  correctColor = Green,
                                  partlyCorrectColor = Yellow,
                                  incorrectColor = Blue,
                                  validGuesses = vg,
                                  possibleAnswers = pa,
                                  showDebug = True }

initializeGame :: Answer -> Game
initializeGame a = Game { answer = a,
                          guesses = [[]],      -- a list containing a single empty item 
                          showInstructions = False,
                          showHints = False,
                          hints = [],
                          helpText = "Enter a 5 letter word (or hit Space to Show/Hide HINTS)" }

-- MAIN
main :: IO ((), Game)
main = do
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

    -- threadDelay 500000 -- Sleep for half a second

    renderLoading config False
    getLine

    runStateT (runReaderT playGame config) game

-- This is the main game loop
playGame :: ReaderT Config (StateT Game IO) ()
playGame = do
    renderGameM
    continue <- processUserInputM
    when continue
        playGame

-- This is the main rendering function that gets called each time the game state has changed
renderGameM :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
renderGameM = do
    game <- get
    config <- ask
    liftIO $ renderGame game config

-- Accepts user input, and updates the Game state as required
-- Returns True: keep playing, False: game has finished
processUserInputM :: (MonadIO m, MonadReader Config m, MonadState Game m) => m Bool
processUserInputM = do
    game <- get
    config <- ask
    let currGuess = currentGuess game
    let guessIsFinished = length currGuess == 5

    if guessIsFinished then do
        liftIO getLine
        evaluateGuessesM
        startNextRowM
    else do
        c <- liftIO getChar
        if c == ' ' then toggleHintsM
        else if c == '!' then toggleInstructionsM -- MO TODO: Ideally want to use Ctrl-I, but apparantly there are issues with terminal support 
        else if c `elem` ['a'..'z'] ++ ['A'..'Z'] then addLetterM (toUpper c)
        else if c == '-' then removeLetterM
        else return () -- still awaiting further user input  -- MO TODO: Use "when"?
    -- MO TODO: Accept backspace character for delete

    -- Check if we've reached game over state
    if gameOver game config then do
        renderGameM
        endGame <- liftIO $ renderGameOver game
        if not endGame then do
            -- Reset the game state
            answer <- liftIO $ selectRandomAnswer (possibleAnswers config)
            let newState = initializeGame answer
            -- MO TODO: How to replace entire state without having to do field by field?
            put (game {
                        guesses = guesses newState,
                        hints = hints newState,
                        showInstructions = showInstructions newState,
                        showHints = showInstructions newState,
                        helpText = helpText newState
                        -- answer = answer newState
                        })
            return True
        else
            return False
    else
        return True

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

addLetterM :: MonadState Game m => Char -> m ()
addLetterM c = do
    game <- get
    let modifiedGuesses = addLetter game c
    put (game { guesses = modifiedGuesses })

removeLetterM :: MonadState Game m => m ()
removeLetterM = do
    game <- get
    let modifiedGuesses = removeLetter game
    put (game { guesses = modifiedGuesses })

evaluateGuessesM :: (MonadReader Config m, MonadState Game m)=> m ()
evaluateGuessesM = do
    game <- get
    let modifiedGuesses = evaluateGuesses game
    put (game { guesses = modifiedGuesses })

startNextRowM :: (MonadReader Config m, MonadState Game m)=> m ()
startNextRowM = do
    game <- get
    let modifiedGuesses = startNextRow game
    put (game { guesses = modifiedGuesses })