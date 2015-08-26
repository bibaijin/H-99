import           System.Random
import Data.List

-- Problem 21: Insert an element at a given position into a list
insertAt :: a -> [a] -> Int -> [a]
insertAt a [] _ = [a]
insertAt a xs 1 = a : xs
insertAt a (x:xs) k = x : insertAt a xs (k - 1)

-- Problem 22: Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range a b = [a..b]

-- Problem 23: Extract a given number of randomly selected elements from a list
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs k = do
    gen <- getStdGen
    return $ take k [xs !! index | index <- randomRs (0, length xs - 1) gen]

-- Problem 24: Draw N different random numbers from the set 1..M
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
    gen <- getStdGen
    return . take n . nub $ randomRs (1, m) gen

-- Problem 25: Generate a random permutation of the elements of a list
rndPermu :: [a] -> IO [a]
rndPermu xs = do
    gen <- getStdGen
    return .
        take
            (length xs)
            [xs !! index | index <- nub $ randomRs (0, length xs - 1) gen]
