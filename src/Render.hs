{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Render (
    renderLoading,
    renderGame,
    renderOver
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
-- MO TODO: Customize background colors as required

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
    putStrLn   "HURDLE:  Haskell version of WORDLE, the addictive game by Josh Wardle."
    putStrLn   "         Available at https://www.nytimes.com/games/wordle"
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


renderHint :: Hint -> IO ()
renderHint x = do
    putStr x
    putStr "  "

renderHints :: Hints -> IO ()
renderHints xs = do
    putStr "HINTS:  "
    mapM_ renderHint xs
    putStrLn ""

-- Renders a 3x3 block at the specified point, with a char in the center, and the specified foreground and background color  
renderBlock :: Point -> Char -> Color -> Color -> IO ()
renderBlock (x,y) c fg bg = do
    setCursorPosition x y
    putStrWithColor "     " fg bg
    setCursorPosition (x+1) y
    putStrWithColor [' ', ' ', c, ' ', ' '] fg bg   -- MO TODO: Improve this!
    setCursorPosition (x+2) y
    putStrWithColor "     " fg bg

-- Renders the guess at the specified point
renderGuess :: Point -> Config -> GuessChar -> IO ()
renderGuess p cfg (c, r) = do
    let fg = fgColorForResult cfg r
    let bg = bgColorForResult cfg r
    renderBlock p c fg bg 

renderRow :: Point -> Config -> Guess -> String -> IO ()
renderRow p cfg [] s = renderRow p cfg emptyGuess s
renderRow (x,y) cfg g s = do
    mapM_ (\(i, gc) -> renderGuess (x, y + i*6) cfg gc) (zip [0..] g)     -- using zip with infinite list allows us to use the index in the lambda
    setCursorPosition (x+1) (y+32)
    putStr s

-- MO TODO: create mapM_withIndex function as I've had to do the zip technique twice

emptyGuess :: Guess
emptyGuess = replicate 5 (' ', None)

-- Creates the specified number of empty guesses
-- Each guess contains 5 letters
emptyGuesses :: Int -> Guesses
emptyGuesses 0 = []
emptyGuesses n = replicate n emptyGuess

helpTextForRow :: Game -> Int -> String
helpTextForRow g i =
    if i == (length gs - 1) then help else "" 
    where gs = guesses g 
          help = helpText g

-- Renders the game board at the specified point
renderBoard :: Point -> Game -> Config -> IO ()
renderBoard (x,y) g c = do

    let gc = maxGuesses c
    let gs = take gc (guesses g ++ emptyGuesses gc)         -- Ensure we render enough empty rows for guesses that have not yet been made
    mapM_ (\(i::Int,guess::Guess) -> renderRow (x+i*4, y) c guess (helpTextForRow g i)) (zip [0..] gs) 
-- MO TODO: create mapM_withIndex function


-- Renders the game as per the current game state
renderGame :: Game -> Config -> IO ()
renderGame g c = do
    clearScreen
    setCursorPosition 0 0
    renderHeader
    renderBoard (5,10) g c
    setCursorPosition (5 + maxGuesses c * 4) 0
    when (showInstructions g) $ renderInstructions c
    when (showHints g) $ renderHints (hints g)

-- Renders the Loading screen at the start of the game
renderLoading :: Config -> Bool -> IO ()
renderLoading c b = do
    clearScreen
    setCursorPosition 0 0
    renderHeader
    renderInstructions c
    if b then putStrLn "Loading..." else putStrLn "Press ENTER to start"


renderOver :: Game -> IO Bool
renderOver g = do
    let winner = wonGame g 
    when winner $ putStrLn ("CONGRATULATIONS: You won in " ++ show (guessCount g) ++ " attempts!")

    putStrLn "Would you like to play again? (Y/N)"
    c <- getChar
    return $ c == 'y' || c == 'Y'

