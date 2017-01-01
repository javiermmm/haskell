droplast :: Int -> [a] -> [a]
droplast x [] = []
droplast 0 xs = xs
droplast x ys = take (lenght ys - x) ys

lenght :: [a] -> Int
lenght [] = 0
lenght (x:xs) = 1+lenght xs

maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

minimo:: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)
