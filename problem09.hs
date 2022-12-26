{-
Problem 9
Special Pythagorean triplet

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a^2 + b^2 = c^2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product a*b*c.
-}

isPythagoreanTriplet :: (Int, Int, Int) -> Bool
isPythagoreanTriplet (a, b, c) =
    a*a + b*b == c*c

-- списки "прямоугольных треугольников" для заданного периметра
triplets :: Int -> [[Int]]
triplets p =
    -- первая сторона не больше периметра/3
    [[a, b, c] | a <- [1..p `div` 3],
                 -- вторая сторона не больше остатка/2
                 b <- [a+1..(p-a) `div` 2],
                 -- третья - остаток
                 let c = p - a - b,
                 isPythagoreanTriplet (a, b, c)]

solution :: Int -> Int
solution target =
  -- перемножить тройку из первого "трегольника" для заданного периметра
  product . head . triplets $ target

main :: IO ()
main =
    putStrLn . show $ (solution 1000)