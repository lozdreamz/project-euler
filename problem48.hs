{-
Problem 48
Self powers

The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
-}

solution :: Integer -> Integer
solution target =
    -- последние 10 цифр - это остаток от деления на 10^10
    mod powersSum (10^10)
    -- просто list comprehension
    where powersSum = sum [n^n | n <- [1..target]]

main :: IO ()
main =
    putStrLn . show . solution $ 1000