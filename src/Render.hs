{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}

module Render (
    renderLoading,
    renderGame,
    renderGameOver
 ) where

import Data.Char
import System.Console.ANSI
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State

import Data
import Game
import Utils

-- Utility functions for rendering

-- Returns the foreground color to use for the specified result
fgColorForResult :: Config -> Result -> Color
fgColorForResult c _ = backgroundColor c

-- Returns the background color to use for the specified result
bgColorForResult :: Config -> Result -> Color
bgColorForResult c None = backgroundColor c
bgColorForResult c Correct = correctColor c
bgColorForResult c PartlyCorrect = partlyCorrectColor c
bgColorForResult c Incorrect = incorrectColor c


-- Renders the specified character multiple times
-- Notes: mapM_ performs the function once per item in the list
--        We ignore the index and just write the character using a lambda
repeatRenderChar :: Char -> Int -> IO ()
repeatRenderChar c n = do
    mapM_ (\_ -> putChar c) [1..n]

-- Writes a string to the screen with the specified foreground and background color  
putStrWithColor :: String -> Color -> Color -> IO ()
putStrWithColor str fg bg = do
    setSGR [SetColor Foreground Vivid fg]
    setSGR [SetColor Background Vivid bg]
    putStr str
    setSGR [Reset]

renderHeader :: IO ()
renderHeader = do
    putStrLn   "--------------------------------------------------------------------------------------------------"
    putStrLn   "HURDLE:  Haskell version of WORDLE: the addictive word guessing game by Josh Wardle."
    putStrLn   "         Original game available at https://www.nytimes.com/games/wordle"
    putStrLn   "--------------------------------------------------------------------------------------------------"

renderInstructions :: Config -> IO ()
renderInstructions cfg = do
    let fgColor = foregroundColor cfg
    let bgColor = backgroundColor cfg
    let corrColor = correctColor cfg
    let incorrColor = incorrectColor cfg
    let partlyCorrColor = partlyCorrectColor cfg

    putStrLn   "--------------------------------------------------------------------------------------------------"
    putStrLn   "INSTRUCTIONS:"
    putStrLn   ""
    putStrLn $ "Guess the WORD in " ++ showNumberText (maxGuesses cfg) ++ " tries"
    putStrLn   "Each guess must be a valid five-letter word. Hit the enter button to submit."
    putStrLn   "After each guess, the color of the tiles will change to show how close your guess was to the word."
    putStrLn   ""
    putStrLn   "Examples:"
    putStrLn   ""
    putStrWithColor "W" fgColor corrColor
    putStr     "EARY     "
    putStrLn   "The letter W is in the word and is in the correct spot"
    putStrLn   ""
    putStr     "P"
    putStrWithColor "I" bgColor partlyCorrColor
    putStr     "LLS     "
    putStrLn   "The letter I is in the word but in the wrong spot"
    putStrLn   ""
    putStr     "VAG"
    putStrWithColor "U" bgColor incorrColor
    putStr     "E     "
    putStrLn   "The letter U is not in the word in any spot"
    putStrLn   ""
    putStrLn   "--------------------------------------------------------------------------------------------------"

-- Renders a 5x5 block at the specified point, with a char in the center, and the specified foreground and background color  
renderBlock :: Point -> Char -> Color -> Color -> IO ()
renderBlock (x,y) c fg bg = do
    setCursorPosition x y
    putStrWithColor "     " fg bg
    setCursorPosition (x+1) y
    putStrWithColor [' ', ' ', c, ' ', ' '] fg bg
    setCursorPosition (x+2) y
    putStrWithColor "     " fg bg

-- Renders the guess at the specified point
renderGuess :: Point -> Config -> GuessChar -> IO ()
renderGuess p cfg (c, r) = do
    let fg = fgColorForResult cfg r
    let bg = bgColorForResult cfg r
    renderBlock p c fg bg

renderRow :: Game -> Config -> Guess -> Point -> String -> IO ()
renderRow game cfg [] p s = renderRow game cfg emptyGuess p s
renderRow game cfg g (x,y) s = do
    let gs = take 5 $ g ++ emptyGuess -- ensure we have 5 columns before rendering
    mapM_ (\(i, gc) -> renderGuess (x, y + i*6) cfg gc) (zip [0..] gs)     -- using zip with infinite list allows us to use the index in the lambda
    setCursorPosition (x+1) (y+32)
    putStr s -- Displays the help text to the right of the row
    when (showHints game && not (null s)) $ do
        let hints = getHints 5 (guesses game) (possibleAnswers cfg)
        unless (null hints) $ do
            setCursorPosition (x+2) (y+32)
            let hnts = unwords hints
            putStr ("HINTS: " ++ hnts)

emptyGuess :: Guess
emptyGuess = replicate 5 (' ', None)

-- Creates the specified number of empty guesses
-- Each guess contains 5 letters
emptyGuesses :: Int -> Guesses
emptyGuesses 0 = []
emptyGuesses n = replicate n emptyGuess

helpTextForRow :: Game -> Config -> Int -> String
helpTextForRow game cfg i =
    if i == (length gs - 1) then help else ""
    where gs = guesses game
          help = helpText game cfg

-- Renders the game board at the specified point
renderBoard :: Point -> Game -> Config -> IO ()
renderBoard (x,y) g c = do
    let gc = maxGuesses c
    let gs = take gc (guesses g ++ emptyGuesses gc)         -- Ensure we render enough empty rows for guesses that have not yet been made
    mapM_ (\(i::Int,guess::Guess) -> renderRow g c guess (x+i*4, y) (helpTextForRow g c i)) (zip [0..] gs)

-- Renders the game as per the current game state
renderGame :: Game -> Config -> IO ()
renderGame g c = do
    clearScreen
    setCursorPosition 0 0
    renderHeader
    renderBoard (5,10) g c
    setCursorPosition (5 + maxGuesses c * 4) 0
    when (showInstructions g) $ renderInstructions c
    when (showDebug c) $ renderDebug g c

-- Renders the Loading screen at the start of the game
renderLoading :: Config -> Bool -> IO ()
renderLoading c b = do
    clearScreen
    setCursorPosition 0 0
    renderHeader
    renderInstructions c
    if b then putStrLn "Loading..." else putStrLn "Press ENTER to start"


renderGameOver :: Game -> IO Bool
renderGameOver g = do
    putStrLn ""
    putStrLn "Would you like to play again? (Y/N)"
    c <- getHiddenChar
    return $ c == 'y' || c == 'Y'

renderDebug :: Game -> Config -> IO ()
renderDebug g c = do
    putStrLn $ show g
    putStrLn $ show c
    let submittedCount = submittedGuessCount g
    putStrLn $ "SubmittedGuessCount=" ++ show submittedCount
    let isOver = gameOver g c
    putStrLn $ "IsGameOver=" ++ show isOver
    let didWinGame = wonGame g
    putStrLn $ "DidWinGame=" ++ show isOver
    putStrLn "Guesses="
    mapM_ (putStrLn . toWord) (guesses g)
    let a = answer g
    putStrLn $ "IsWinningGuess=" ++ a
    mapM_ (putStrLn . show . winningGuess a) (guesses g)
    putStrLn $ "CurrentGuessIsValid=" ++ show (currentGuessIsValid c g)
