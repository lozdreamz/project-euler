{-
Problem 7
10001st prime

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
What is the 10 001st prime number?
-}

-- из задачи #3

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
   primes !! target 

main :: IO ()
main =
    -- помним, что нумерация с 0, а число нужно 10001-е
    putStrLn . show $ (solution 10000)