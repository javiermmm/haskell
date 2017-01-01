ej15 :: Int -> Int
ej15 x = numBitPos (abs x)

numBitPos :: Int -> Int
numBitPos 0 = 1
numBitPos 1 = 1
numBitPos x = 1 + numBitPos (x `div` 2)

ej16 :: Int -> [Int]
ej16 x = reverse (binInv x)

binInv 0 = [0]
binInv 1 = [1]
binInv x = x `mod` 2 : binInv (x `div` 2)

ej17a :: Ord a => [a] -> [a]
ej17a [] = []
ej17a xs = min : ej17a (quita min xs)
  where min = minimo xs

quita :: Eq a => a -> [a] -> [a]
quita x [] = []
quita x (y:ys)
  | x == y = ys
  | otherwise = y : (quita x ys)

minimo :: Ord a => [a] -> a
minimo (x:xs) = minimoAcum x xs

minimoAcum :: Ord a => a -> [a] -> a
minimoAcum x [] = x
minimoAcum x (y:ys)
  | y < x = minimoAcum y ys
  | otherwise = minimoAcum x ys

menYMay :: Ord a => a -> [a] -> ([a],[a])
menYMay x [] = ([], [])
menYMay x (y:ys)
  | y <= x = (y:men, may)
  |otherwise = (men, y:may)
  where (men,may) = menYMay x ys

ej17b :: Ord a => [a] -> [a]
ej17b [] = []
ej17b (x:xs)  = (ej17b men) ++ [x] ++ (ej17b may)
  where (men, may) = menYMay x xs