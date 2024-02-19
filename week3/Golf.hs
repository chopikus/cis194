module Golf where
import Data.List (intercalate)
import Data.Char (chr)

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

-- maxDigitOccurence+1 characters is the height of histogram
histogram :: [Int] -> String
histogram els = (intercalate "\n" (map (histRow els) [h, h-1..(-1)]))
                  where h = maxDigitOccurence els

-- given a list and a row coordinate, render that row
-- 0 refers to the bottow row with ======
histRow :: [Int] -> Int -> String
histRow els y = map (\x -> (histChar els x y)) [0..9]

-- Given an original list, determines a character at a specific point of a histogram 
-- _,0 refers to the bottom row with =======
-- 0,1 refers to left-most character of 2nd row from the bottom
histChar :: [Int] -> Int -> Int -> Char
histChar _ x (-1) = "0123456789" !! (x `mod` 10)
histChar _ _ 0 = '='
histChar els x y = case ((occurence els x) >= y) of
                     True -> '*'
                     False -> ' '

-- returns the maximum number of times some digit is used in the list.
maxDigitOccurence :: [Int] -> Int
maxDigitOccurence xs = maximum $ (map (occurence xs) [1..9])

-- returns the number of time sime element of a list occurs in it
occurence :: [Int] -> Int -> Int
occurence xs e = (length . filter (\x -> x == e)) xs
