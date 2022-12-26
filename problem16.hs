{-
Problem 16
Power digit sum

2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
What is the sum of the digits of the number 2^1000?
-}

-- сумма цифр числа
sumDigits :: Integer -> Integer
sumDigits n =
    sumDigits' n 0
        -- суммирование с использованием аккумулятора
        where   sumDigits' 0 acc = acc
                sumDigits' n acc = sumDigits' (n `div` 10) (acc + (n `mod` 10))

solution :: Integer -> Integer
solution target =
    sumDigits target

main :: IO ()
main =
    putStrLn . show . solution $ (2^1000)