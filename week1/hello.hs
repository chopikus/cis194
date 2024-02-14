main = do
  putStrLn "Hello, everybody!"
  putStrLn ("Please look at my favourite odd numbers: " ++ show (filter odd [10..20]))
