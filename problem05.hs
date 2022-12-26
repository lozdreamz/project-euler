{-
Problem 5
Smallest multiple

520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}


solution :: Int -> Int
solution target =
    -- просто найти НОК (LCM) для каждой пары чисел, начиная с конца списка
    -- найденое НОК становится парой следующему элементу списка
    foldr1 lcm [2..target]

main :: IO ()
main =
    putStrLn . show $ (solution 20)