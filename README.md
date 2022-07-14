# HURDLE:  Haskell version of WORDLE, the addictive word-guessing game by Josh Wardle.

## Original version available at https://www.nytimes.com/games/wordle

This version written by Matthew O'Leary as a project for the Emurgo Cardano Developer course - Batch 59
***
### HOW TO PLAY

Guess the WORD in six tries.

Each guess must be a valid five-letter word. Hit the enter button to submit.

After each guess, the color of the tiles will change to show how close your guess was to the word.

Examples:

<span style="color:black;background-color:green">W</span>EARY :          &nbsp;  The letter W is in the word and is in the correct spot

P<span style="color:black;background-color:yellow"> I </span>LLS : &nbsp;&nbsp;  The letter I is in the word but in the wrong spot

VAG<span style="color:black;background-color:grey">U</span>E :           &nbsp;  The letter U is not in the word in any spot

***

### INSTRUCTIONS
Hurdle is written in Haskell and requires the Glasgow Haskell Compiler to be installed in order to build the project.

The project is a Cabal project and therefore requires Cabal to be installed.  

The following commands should be run in terminal or other command line interface, from within the project directory.

To build:

    cabal build

To run unit tests:

    cabal test

To run:

    cabal run Hurdle
***

Source available at:
https://github.com/teevus/Hurdle
