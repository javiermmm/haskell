ej9 :: [Int] -> [Int] -> Int
ej9 [] (y:ys) = 0
ej9 (x:xs) [] = 0
ej9 [] [] = 0
ej9 (x:xs) (y:ys) = x*y + ej9 xs ys

ej10 :: [a] -> [a] -> [a]
ej10 [] ys = ys
ej10 (x:xs) ys = x : ej10 xs ys

concatena :: [[a]] -> [a]
concatena [] = []
concatena (x:xs) = x++concatena xs

ej12 :: Ord a => [a] -> Bool
ej12 [] = False
ej12 (x:[]) = True
ej12 (x:y:xs)
  | x <= y = ej12 (y:xs)
  | otherwise = False

ej13a :: [Int] -> [Int]
ej13a [] =[]
ej13a (x:xs)
  | even x == True = ej13a xs
  | otherwise = x:ej13a xs

ej13b :: [Int] -> [Int]
ej13b [] = []
ej13b (x:xs)
  | even x == True = x:ej13b xs
  | otherwise = ej13b xs