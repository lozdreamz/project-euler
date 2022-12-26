{-
Problem 3
Largest prime factor

The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?
-}

{-# LANGUAGE NumericUnderscores #-}

-- бесконечный список простых чисел
primes :: [Int]
-- 2 + фильтруем бесконечный список нечетных чисел: взять те, у которых
-- список простых множителей не имеет хвоста (в списке один элемент - само это число)
primes =
    2 : filter (null . tail . primeFactors) [3,5..]

-- список простых множителей числа
primeFactors :: Int -> [Int]
primeFactors n =
    factor n primes
    where
        factor n (p : ps) 
            -- дошли до квадратного корня из числа
            | p * p > n = [n]
            -- делится на простое число, продолжаем делить
            | n `mod` p == 0 = p : factor (n `div` p) (p : ps)
            -- идем на следующее простое число
            | otherwise = factor n ps

solution :: Int -> Int
solution target =
    last . primeFactors $ target

main :: IO ()
main =
    putStrLn . show $ (solution 600_851_475_143)