{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-
This module contains functions that operate on the Game and related data types such as Guess etc.
This module is for (non-IO related) game logic, and should only contain pure functions (no IO monad).
Rendering logic is in Render.hs, not in this module
-}
module Game (
    submittedGuesses,
    wonGame,
    gameOver,
    toWord,
    submittedGuessCount,
    currentGuess,
    currentGuessIndex,
    addLetter,
    removeLetter,
    evaluateGuesses,
    startNextRow,
    guessIsSubmitted,
    winningGuess,
    initializeConfig,
    initializeGame,
    processUserInput,
    submitGuess,
    guessIsValid,
    currentGuessIsValid,
    guessIsFinished,
    currentGuessIsFinished,
    currentGuessIsSubmitted,
    processEnterKey,
    helpText,
    getHints
) where

import System.Console.ANSI
import Data.Char
import Data.List
import Data
import Utils

-- Initializes the config
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
                                  showDebug = False }

-- Initializes the game data with the specified Answer
initializeGame :: Answer -> Game
initializeGame a = Game { answer = a,
                          guesses = [[]],      -- a list containing a single empty item 
                          showInstructions = False,
                          showHints = False,
                          hints = [],
                          userQuit = False,
                          showInvalidMessage = False }

-- Returns the guesses that have been submitted
submittedGuesses :: Game -> [Guess]
submittedGuesses g = filter guessIsSubmitted (guesses g)

-- Retrieves the number of guesses that have been submitted
submittedGuessCount :: Game -> Int
submittedGuessCount g = length $ submittedGuesses g

-- Retrieves the (zero-based) index of the current row being guessed
currentGuessIndex :: Game -> Int
currentGuessIndex g = if gc == 0 then 0 else gc - 1
    where gc = length $ guesses g

currentGuess :: Game -> Guess
currentGuess g = last $ guesses g

-- Determines whether a particular guess has been submitted
guessIsSubmitted :: Guess -> Bool
guessIsSubmitted g = guessIsFinished g && all (\(_,r) -> r /= None) g

-- Determine whether the current guess has been submitted
currentGuessIsSubmitted :: Game -> Bool
currentGuessIsSubmitted g = guessIsSubmitted (currentGuess g)

-- Determines whether the game is finished (i.e. maximum number of guesses have been submitted)
-- This will return true if the game is finished regardless of whether the game was won or lost
gameOver :: Game -> Config -> Bool
gameOver g c = submittedGuessCount g >= maxGuesses c || any (winningGuess (answer g)) (guesses g)

-- Determines whether the player has won the game
wonGame :: Game -> Bool
wonGame g = any (winningGuess (answer g)) (submittedGuesses g)

-- Determine whether the specified guess is a winning guess
winningGuess :: Answer -> Guess -> Bool
winningGuess a g = guessIsSubmitted g && toWord g == a

-- Converts a guess (which is a list of tuples) to the actual word that was guessed (String)
toWord :: Guess -> String
toWord g = map fst g

-- Adds a letter to the current guess and returns a list containing the guesses
addLetter :: Game -> Char -> Game
addLetter game c = game { guesses = replaceElem gs (currentGuessIndex game) modifiedGuess }
    where gs = guesses game
          currGuess = currentGuess game
          modifiedGuess = addLetterToGuess currGuess c

-- Removes the last letter from the current guess and returns the updated game state
-- Note: the last letter can only be removed if the guess has not been submitted yet (i.e. Result will be None)
removeLetter :: Game -> Game
removeLetter game = game { guesses = replaceElem gs (currentGuessIndex game) modifiedGuess, showInvalidMessage = False }
    where gs = guesses game
          currGuess = currentGuess game
          modifiedGuess = removeLetterFromGuess currGuess

-- Adds a letter to the specified guess (unless its already full)
addLetterToGuess :: Guess -> Char -> Guess
addLetterToGuess g c = if length g < 5 then g ++ [(c, None)] else g
-- Note: adding to the end of a list in Haskell can be expensive, but in this case our list will have maximum length of 6

-- Removes the last letter from the guess
removeLetterFromGuess :: Guess -> Guess  -- Note: Guess is simply [GuessChar]
removeLetterFromGuess [] = []
removeLetterFromGuess gc = init gc

-- Evaluates all guesses and returns update Game with the Result values populated for the submitted guesses
evaluateGuesses :: Game -> Game
evaluateGuesses game = game { guesses = map (evaluateGuess (answer game)) (guesses game) }

-- Evaluates the specified guess, returning a Guess type which is a list of GuessChar values with the Result value populated
evaluateGuess :: Answer -> Guess -> Guess
-- evaluateGuess :: Answer -> [GuessChar] -> [GuessChar]
evaluateGuess a gcs = map (\(i,(c,_)) -> evaluateGuessChar a i c) (zip [0..] gcs)

evaluateGuessChar :: Answer -> Int -> Char -> GuessChar
evaluateGuessChar a index c
    | a !! index == c  = (c,Correct)
    | c `elem` a       = (c,PartlyCorrect)
    | otherwise        = (c,Incorrect)

-- Submits the guess, which evaluates it, and if its a valid guess, will start a new row 
submitGuess :: Game -> Config -> Game
submitGuess game cfg = 
    if currentGuessIsValid cfg game then
        startNextRow (evaluateGuesses game { showInvalidMessage = False }) cfg  
    else 
        game { showInvalidMessage = True }

-- Checks if the guess is a valid one using the list of valid words
guessIsValid :: Config -> Guess -> Bool
guessIsValid c g = guessIsFinished g && toWord g `elem` validGuesses c

currentGuessIsValid :: Config -> Game -> Bool
currentGuessIsValid c g =  guessIsValid c $ currentGuess g

guessIsFinished :: Guess -> Bool
guessIsFinished g = length g == 5

currentGuessIsFinished :: Game -> Bool
currentGuessIsFinished g = guessIsFinished (currentGuess g)

-- Starts a new row on the board (if its not already at gameOver state)
startNextRow :: Game -> Config -> Game
startNextRow game cfg
    | gameOver game cfg = game -- Do nothing
    | otherwise = game { guesses = guesses game ++ [[]], showHints = False }

-- Text on the current row that tells the user what they need to do
helpText :: Game -> Config -> String
helpText game cfg
    | gameOver game cfg           = if wonGame game then 
                                        "CONGRATULATIONS: You won in " ++ show (submittedGuessCount game) ++ " attempts!"
                                    else
                                        "BAD LUCK: You ran out of attempts!"
    | showInvalidMessage game     = "You have entered an invalid word!"
    | currentGuessIsFinished game = "Press ENTER to Submit"
    | otherwise                   = "Enter a 5 letter word"

-- Determines whether the specified string matches the guessed character
matchesGuess :: String -> GuessChar -> Bool
-- matchesGuess _ (_, None) = False -- I'd prefer an error since this shouldnt ever be passed to this function
matchesGuess s (c, Incorrect) = c `notElem` s      -- notElem returns True if the element is not contained within the list (from Data.List)
matchesGuess s (c, PartlyCorrect) = c `elem` s     -- elem return True if the element is contained within the list (from Data.List)
matchesGuess s (c, Correct) = c `elem` s

-- Determines whether the specified string matches the list of guessed characters
matchesGuesses :: [GuessChar] -> String -> Bool
matchesGuesses [] s = True
matchesGuesses xs s = all (matchesGuess s) xs

-- select n words from the list of possible answers that contain the known letters in the correct place
getHints :: Int -> Guesses -> [Answer] -> [String]
getHints n gs pa = take n (filter (matchesGuesses g) pa)
                   where g = knownResults gs

-- Collates what we know from the results of the guesses thus far into a structure thats more usable
knownResults :: Guesses -> [GuessChar]
knownResults [] = []
knownResults gs = filter (\(_,r) -> r /= None) (nub $ concat gs)

-- Processes the user input character
processUserInput :: Char -> Game -> Config -> (Bool, Game)
processUserInput c game cfg
  | c == ' '               = (True, game { showHints = not $ showHints game })
  | c == '+'               = (True, game { showInstructions = not $ showInstructions game })
  | c `elem` ['a'..'z'] || 
    c `elem` ['A'..'Z']    = (True, addLetter game (toUpper c))     -- Valid letter
  | fromEnum c == 8        = (True, removeLetter game)              -- Backspace
  | fromEnum c == 13       = (True, processEnterKey game cfg)       -- Enter
  | fromEnum c == 27       = (True, game { userQuit = True })       -- Esc
  | otherwise              = (False, game)                          -- Invalid input

-- Processes the enter key being pressed
processEnterKey :: Game -> Config -> Game
processEnterKey game cfg =
    if currentGuessIsFinished game && not (currentGuessIsSubmitted game) then
        submitGuess game cfg
    else game
