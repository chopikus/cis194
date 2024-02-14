-- credit card validation

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : (toDigitsRev (n `div` 10))

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = (toDigits (n `div` 10)) ++ [n `mod` 10]

listRev :: [Integer] -> [Integer]
listRev [] = []
listRev (x : xs) = (listRev xs) ++ [x]

doubleOther :: [Integer] -> [Integer]
doubleOther [] = []
doubleOther [a] = [a]
doubleOther (a : b : x) = a : (b * 2) : doubleOther x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = (listRev . doubleOther . listRev) xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs)
  | x < 10 = x + sumDigits xs
  | x >= 10 = (x `mod` 10) + (x `div` 10) + sumDigits xs

validate :: Integer -> Bool
validate x = mod ((sumDigits . doubleEveryOther . toDigits) x) 10 == 0

type Peg = String
type Move = (String, String)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- first peg "from"
-- second peg "to"
-- third peg "temp"
hanoi 0 _ _ _ = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)
