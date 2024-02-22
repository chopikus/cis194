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
