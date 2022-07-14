module Utils (
    selectRandomItem,
    selectRandomItems,
    showNumberText,
    replaceElem,
    parseWords
) where

import System.Random
import Data.List
import Data.Char

-- Selects an item at random from the specified list
selectRandomItem :: StdGen -> [a] -> a
selectRandomItem gen xs = xs !! rand 
    where (rand, _) = randomR (0, length xs - 1) gen 

-- Selects n items at random from the specified list
selectRandomItems :: StdGen -> Int -> [a] -> [a]
selectRandomItems _ _ [] = []
selectRandomItems _ 0 _ = []
selectRandomItems gen n xs = [xs!!i | i <- take n . nub $ (randomRs (1,length xs) gen :: [Int])] 
-- Notes:
-- nub removes duplicate items from the list of indexes we have selected at random
-- we use list comprehension to select by index.  This may not be very efficient for larger lists since each use of !! is O(n)
-- Our list of indexes will be small (e.g. up to 100) so performance should be ok

-- Converts specified Num value into a textual representation 
-- (up to 'ten' as full number text, or the numeric value as a String if greater than 10)
showNumberText :: (Eq a, Num a, Show a) => a -> String
showNumberText 1 = "one"
showNumberText 2 = "two"
showNumberText 3 = "three"
showNumberText 4 = "four"
showNumberText 5 = "five"
showNumberText 6 = "six"
showNumberText 7 = "seven"
showNumberText 8 = "eight"
showNumberText 9 = "nine"
showNumberText 10 = "ten"
showNumberText n = show n

-- Returns a list containing all items in the provided list, 
-- but with the item at the specified index having been replaced with the provided item
replaceElem :: [a] -> Int -> a -> [a]
replaceElem [] _ _ = []
replaceElem (_:xs) 0 a = a:xs
replaceElem (x:xs) n a =
    if n < 0 then (x:xs) else (x : replaceElem xs (n-1) a)

-- Parses the string to seperate the individual 5 letter words
parseWords :: String -> [String]
parseWords s = map (map toUpper) wrds
    where wrds = map (take 5) (words s)