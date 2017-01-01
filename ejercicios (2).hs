ej9 :: [Int] -> [Int] -> Int
ej9 [] ys = 0
ej9 xs [] = 0
ej9 (x:xs) (y:ys) = x * y + ej9 xs ys


ej10 :: [a] -> [a] -> [a]
ej10 [] ys = ys
ej10 (x:xs) ys = x: ej10 xs ys


concatena :: [[a]] -> [a]
concatena [] = []
concatena (x:xs) = x++concatena xs


ej12 :: Ord a => [a] -> Bool
ej12 [] = True
ej12 [x] = True
ej12 (x:y:xs) = (x <= y) && ej12 (y:xs)
  

ej13 :: [Int] -> [Int]
ej13 [] = []
ej13 (x:xs)
  |odd x = x:ej13 xs
  |otherwise = ej13 xs

ej13b :: [Int] -> [Int]
ej13b [] = []
ej13b (x:xs)
  |even x = x:ej13b xs
  |otherwise = ej13b xs

coloca :: [Int] -> [Int]
coloca [] = []
coloca xs = mezcla pares impares
  where pares = ej13 xs
        impares = ej13b xs
mezcla :: [Int] -> [Int] -> [Int]
mezcla xs [] = []
mezcla [] ys = []
mezcla (x:xs) (y:ys) = x:y:mezcla xs ys
