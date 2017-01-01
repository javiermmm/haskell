--3.)

tresDiferentes:: Int -> Int -> Int -> Bool
tresDiferentes x y z = if (x /= y) && (x /= z) && (y /= z)  then True else False

--4.)

max3:: Int -> Int -> Int -> Int
max3 x y z = max (max x y) z

--5.)

cuantosIguales:: Int -> Int -> Int -> Int
cuantosIguales x y z = if tresDiferentes x y z then 1
		       else if (x == y) && (y == z) then 3
		       else 2
		       
--cuantosIguales:: Int -> Int -> Int -> Int
--cuantosIguales x y z = if (x == y) && (y == z) then 3
			 --else (x == y || y == z || x== z) then 2
			 --else 1

sumatorio:: Int -> Int
sumatorio 1 = 1
sumatorio x = x + sumatorio (x-1)

--sumatorio2:: Int -> Int
--sumatorio2 1 = 1
--sumatorio2 x = sumatorio2 () 

raices:: Float -> Float -> Float -> (Float, Float)
raices a b c = ((-b + disc)/denom, (-b -disc)/denom)
	where
		disc
			|calc < 0 = error "raices no reales"
			|otherwise = sqrt calc
			where
				calc = b^2 - 4*a*c
		denom = 2*a

escalar:: [Int] -> [Int] -> Int
escalar _ [] = 0
escalar [] _ = 0
escalar (x:xs) (y:ys) = x*y + escalar xs ys
	
--concatena:: [[a]] -> [a]
--concatena [] = []
--concatena [(x:xs)] = [concatena x ++ concatena xs]
--CONCATENA NO FUNCIONA

ordenada:: [Int] -> Bool
ordenada [] = True
ordenada [x] = True
ordenada (x:y:xs) = (x < y) && ordenada (y:xs)

quitaPares:: [Int] -> [Int]
quitaPares [] = []
quitaPares [x]
	|even x = []
	|otherwise = [x]
quitaPares (x:xs) = quitaPares xs 
--QUITAPARES NO FUNCIONA