import qualified Data.Set as S

fun1' :: [Integer] -> Integer
fun1' = (foldr (*) 1) . map (\x -> x-2) . (filter even)


fun2' :: Integer -> Integer
fun2' = foldr (+) 0
        . filter even 
        . takeWhile (>1)
        . iterate (\x -> if (even x) then (x `div` 2)
                                     else (3*x+1))


-- Integer in Node representing height
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)


-- Generate a balanced binary tree from a sorted list
-- Note that the generated tree is NOT a binary search tree
foldTree :: [a] -> Tree a
foldTree xs = foldr appendTree Leaf xs

-- Appends one item to the tree keeping it balanced.
appendTree :: a -> Tree a -> Tree a
appendTree x Leaf = Node 0 Leaf x Leaf
appendTree x (Node h l c r) = if (treeHeight l < treeHeight r)
                              then let nl = appendTree x l
                                       nh = 1 + max (treeHeight nl) (treeHeight r)
                                   in (Node nh nl c r)

                              else let nr = appendTree x r
                                       nh = 1 + max (treeHeight l) (treeHeight nr)
                                   in (Node nh l c nr)


-- Tree height of Node is >=0
treeHeight :: Tree a -> Integer
treeHeight Leaf = (-1)
treeHeight (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x y -> (f x : y) ) []


-- given 2 lists, generates their cartesian product
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- given a list, generate a cartesian product with itself
cartSq :: [a] -> [(a, a)]
cartSq xs = cartProd xs xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (map (\x -> x*2+1)
                   . filter (`S.notMember` toRemove)) [1..n]
                  where 
                       toRemove = S.fromList (sundaramRemove n)

-- Given n, generate numbers x such that (x<=n AND exists i,j. (i<=j, x=i+j+2ij))
-- Some elements may repeat
sundaramRemove :: Integer -> [Integer]
sundaramRemove n = foldr (++) []
                   $ map (\j -> map (\i -> i+j+2*i*j) (sundaramRemoveOne j n))
                   [1 .. n]

-- for given j and n, generate i such that (i<=j AND i+j+2ij <= n)
sundaramRemoveOne :: Integer -> Integer -> [Integer]
sundaramRemoveOne j n = takeWhile (\i -> i<=j && i+j+2*i*j<=n) $ iterate (+1) 1

