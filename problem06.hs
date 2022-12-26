{-
Problem 6
Sum square difference

The sum of the squares of the first ten natural numbers is,
1^2 + 2^2 + ... + 10 ^2 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... 10)^2 = 55^2 = 3025

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is
3025 - 385 = 2640

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
-}


bruteforceSolution :: Int -> Int
bruteforceSolution target =
    -- разность между квадратом суммы и суммой квадратов
    (sum [1..target])^2 - sum (map (^2) [1..target])

solution :: Int -> Int
solution target =
    -- квадрат суммы по формуле (n*(n+1)/2) ^2
    -- сумма квадратов по формуле n*(n+1)*(2*n+1) / 6
    (target * (target + 1) `div` 2)^2 - (target * (target + 1) * (2*target+1) `div` 6)

main :: IO ()
main = do
    putStrLn . show $ (bruteforceSolution 100)
    putStrLn . show $ (solution 100)