ej15 :: Int -> Int
ej15 x = lenght (convierte x)+1

convierte :: Int -> [Int]
convierte 0 = [0]
convierte 1 = [1]
convierte (-1) = [1]
convierte x = convierte (absoluto(x) `div` 2)++[absoluto (x) `mod` 2]

lenght :: [a] -> Int
lenght [] = 0
lenght (x:xs) = 1 + lenght xs

absoluto :: Int -> Int
absoluto 0 = 0
absoluto x
  |x<0 = x - (2*x)
  |otherwise = x

ej16 :: Int -> [Int]
ej16 0 = [0]
ej16 1 = [1]
ej16 x = ej16 cociente++[resto]
  where cociente = x `div` 2
        resto = x `mod` 2