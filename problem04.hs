{-
Problem 4
Largest palindrome product

A palindromic number reads the same both ways.
The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
Find the largest palindrome made from the product of two 3-digit numbers.
-}


-- проверка, что число является палиндромом
isPalindrome :: Int -> Bool
isPalindrome n =
    -- придется использовать преобразование в строку и reverse
    show n == (reverse . show $ n)

solution :: Int
solution =
    -- наибольший элемент из списка палиндромов
    maximum [x*y | x <-[100..999], y <-[x..999], isPalindrome (x*y)]


main :: IO ()
main =
    putStrLn . show $ solution