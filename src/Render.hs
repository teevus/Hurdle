module Render (
    renderGame
 ) where

import Data.Char
import System.Console.ANSI
import Control.Monad
import Data

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

writeNumber :: Int -> String
writeNumber 1 = "one"
writeNumber 2 = "two"
writeNumber 3 = "three"
writeNumber 4 = "four"
writeNumber 5 = "five"
writeNumber 6 = "six"
writeNumber 7 = "seven"
writeNumber 8 = "eight"
writeNumber 9 = "nine"
writeNumber 10 = "ten"
writeNumber n = show n

renderInstructions :: Config -> IO ()
renderInstructions c = do
    putStrLn   "--------------------------------------------------------------------------------------------------"
    putStrLn   "HURDLE:  Haskell version of WORDLE, the addictive game by Josh Wardle."
    putStrLn   "Available at https://www.nytimes.com/games/wordle"
    putStrLn   "--------------------------------------------------------------------------------------------------"
    putStrLn   ""
    putStrLn $ "Guess the WORD in " ++ writeNumber (guessCount c) ++ " tries"
    putStrLn   "Each guess must be a valid five-letter word. Hit the enter button to submit."
    putStrLn   "After each guess, the color of the tiles will change to show how close your guess was to the word."
    putStrLn   ""
    putStrLn   "Examples:"
    putStrLn   ""
    putStrWithColor "W" Black Green
    putStr     "EARY     "
    putStrLn   "The letter W is in the word and is in the correct spot"
    putStrLn   ""
    putStr     "P"
    putStrWithColor "I" Black Yellow
    putStr     "LLS     "
    putStrLn   "The letter I is in the word but in the wrong spot"
    putStrLn   ""
    putStr     "VAG"
    putStrWithColor "U" Black White
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
    putStrWithColor "   " fg bg
    setCursorPosition (x+1) y
    putStrWithColor [' ', c, ' '] fg bg
    setCursorPosition (x+2) y
    putStrWithColor "   " fg bg

-- Renders the guess at the specified point
renderGuess :: Point -> Config -> GuessChar -> IO ()
renderGuess (x, y) cfg (c, r) = do
    let fg = fgColorForResult cfg r
    let bg = bgColorForResult cfg r
    return ()

renderEmptyRow :: Point -> Config -> IO ()
renderEmptyRow p cfg = do
    renderGuess p cfg (' ', None) 

renderRow :: Point -> Config -> Guess -> IO ()
renderRow p cfg [] = renderEmptyRow p cfg
renderRow (x,y) cfg [gc] = do
    mapM_ (\i -> renderGuess (x + i*4, y) cfg gc) [0..2] 


-- Creates the specified number of empty guesses
-- Each guess contains 5 letters
emptyGuesses :: Int -> Guesses
emptyGuesses 0 = []
emptyGuesses n = replicate n $ replicate 5 (' ', None)

-- Renders the game board at the specified point
renderBoard :: Point -> Game -> Config -> IO ()
renderBoard p g c = do
    let gc = guessCount c
    let gs = take gc (guesses g ++ emptyGuesses gc)         -- Ensure we render enough empty rows for guesses that have not yet been made
    mapM_ (renderRow p c) gs

-- This is the main rendering function that gets called each time the game state has changed
renderGame :: Game -> Config -> IO ()
renderGame g c = do
    clearScreen
    -- renderBoard (10,10) g c
    when (showInstructions g) $ renderInstructions c
    when (showHints g) $ renderHints (hints g)
