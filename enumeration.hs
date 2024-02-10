data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show


shoe :: Thing
shoe = Shoe

data OptionalDouble = None
                    | Some Double

ex01 = None
ex02 = Some 3.4


data FailableDouble = Failure
                    | OK Double

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

nameField :: Person -> String
nameField p@(Person n _ _) = "The name of (" ++ show p ++ ") is " ++ n

patternMatch = case "Hello" of
                    [] -> 3
                    ('H' : s) -> length s
                    _ -> 7

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                        Failure -> 0
                        OK d -> d

data IntList = Empty
             | Cons Int IntList
  deriving Eq

intListProduct :: IntList -> Int
intListProduct Empty = 1
intListProduct (Cons x _) = 0
