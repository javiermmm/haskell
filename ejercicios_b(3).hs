takelast :: Int -> [a] -> [a]
takelast x [] = []
takelast 0 xs = []
takelast x (y:ys) = take x (reverse (y:ys))

genFib :: Int -> Int -> [Int]
genFib x y = x : genFib y (x+y)

lenght :: [a] -> Int
lenght [] = 0
lenght (x:xs) = 1+lenght xs

ej20 :: Int -> Int
ej20 0 = 1
ej20 x = head (reverse(take x (genFib 1 2)))