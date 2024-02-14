data List t = E | C t (List t)
  deriving Show

lst1 :: List Int
lst1 = C 2 (C 3 (C 5 E))

lst2 :: List (Int, Bool)
lst2 = C (2, True) (C (3, False) E)

filterList _ E = E
filterList pr (C x xs)
  | pr x = C x (filterList pr xs)
  | otherwise = (filterList pr xs)

mapList :: (a -> b) -> List a -> List b
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)


data NonEmptyList a = NEL a [a]
  deriving Show

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x : xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel [] = Nothing
listToNel (x : xs) = Just $ NEL x xs
