module Render (
    renderBoard,
    renderInstructions,
    renderHints
 ) where

import Data.Char
import System.Console.ANSI
import Data

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
renderInstructions (Config c) = do
    putStrLn   "--------------------------------------------------------------------------------------------------"
    putStrLn   "HURDLE:  Haskell version of WORDLE, the addictive game by Josh Wardle."
    putStrLn   "Available at https://www.nytimes.com/games/wordle"
    putStrLn   "--------------------------------------------------------------------------------------------------"
    putStrLn   ""
    putStrLn $ "Guess the WORD in " ++ writeNumber c ++ " tries"
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

-- Renders a horizontal line starting at the current cursor position
renderHorizontalLine :: Int -> IO ()
renderHorizontalLine n = do
    repeatRenderChar (chr 196) n

renderTopLeft :: IO ()
renderTopLeft = do
    putChar $ chr 169

renderVerticalLine = do
    putChar $ chr 179

renderBoard :: Game -> Config -> IO ()
renderBoard g c = do
    renderVerticalLine
    putStrLn ""

    renderHorizontalLine 5
    putStrLn ""

    renderTopLeft
    putStrLn ""
    return ()