module Golf where

-- First, we make an array of skip sizes
-- Then we create an array of indices for each skip
-- Then we map each index to it's element in xs
-- Repeat for every skip size
skips :: [a] -> [[a]]
skips xs = map (map (\i -> xs !! i) . everyNth (length xs)) [1..length xs]

-- Creates an array of elements [n, 2n, 3n, ..]
everyNth :: Int -> Int -> [Int]
everyNth _ 0 = []
everyNth x n = filter (\x -> (x+1) `mod` n == 0) [0..x-1]

-- Creates a window of 3 three elements for each index
-- Filters each window where a middle element is a maxima
-- Leaves a middle element in filtered windows
localMaxima :: [Integer] -> [Integer]
localMaxima xs = (map (\(a,b,c) -> b) . filter maxima . map (window3 xs) ) [0..(length xs) - 3]

-- Creates a window of 3 from array and index
-- Can crash with a wrong index
window3 :: [Integer] -> Int -> (Integer, Integer, Integer)
window3 xs i = (xs!!i, xs!!(i+1), xs!!(i+2))

-- Checks whether a middle element of triple is a maxima
maxima :: (Integer, Integer, Integer) -> Bool
maxima (a, b, c) = (b > a && b > c)
