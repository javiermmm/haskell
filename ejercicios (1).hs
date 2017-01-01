import Hugs.Prelude

f :: a -> a
f x = x

ej1 :: Float -> Float
ej1 x = 3.14159 * x * x

ej2 :: Integer -> Bool
ej2 x = ceiling y == floor y
  where y = sqrt (fromInteger x)

ej2b :: Int -> Bool
ej2b x = ceiling y == floor y
  where y = sqrt (fromInt x)

ej2c :: Int -> Bool
ej2c x = ceiling y == floor y
  where y = sqrt (fromInteger (toInteger x))

ej3 :: Float -> Float -> Float -> Int
ej3 a b c
  | d < 0 = 0
  | d == 0 = 1
  | d > 0 = 2
  where d = b * b - 4 * a * c

ej7 :: [a] -> [b] -> [c] -> [(a,b,c)]
ej7 [] ys zs = []
ej7 xs [] zs = []
ej7 xs ys [] = []
ej7 (x:xs) (y:ys) (z:zs) = (x,y,z):ej7 xs ys zs

ej8 :: [Int] ->Int
ej8 [] = 1
ej8 (x:xs) = x * ej8 xs