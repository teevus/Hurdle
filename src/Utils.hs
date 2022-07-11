module Utils (
    selectRandomItem,
    selectRandomItems
) where

import System.Random
import Data.List

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