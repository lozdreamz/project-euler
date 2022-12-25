{-
Problem 1
Multiples of 3 or 5

If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.
-}

-- решение с помощью простого списка
simpleListSolution :: Int -> Int
simpleListSolution target =
	-- сумма чисел, кратных 3 или кратных 5
	sum [x | x <- [1..target], mod x 3 == 0 || mod x 5 == 0]

-- решение с помощью списков кратных чисел
multSolution :: Int -> Int
multSolution target =
	let sumMult n =
		-- сумма чисел кратных n
		n * p * (p + 1) `div` 2
		where p = target `div` n
	in
	-- кратные 3 + кратные 5 - кратные 3 и 5
	sumMult 3 + sumMult 5 - sumMult 15

main :: IO ()
main = do
	-- вывести результаты обоих решений
	putStrLn . show $ (simpleListSolution target)
	putStrLn . show $ (multSolution target)
	where
		target = 1000 - 1