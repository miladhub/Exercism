module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum $ filter (multipleOf factors) [1..(limit - 1)]

multipleOf :: [Integer] -> Integer -> Bool
multipleOf factors n = any (divides n) factors

divides :: Integer -> Integer -> Bool
divides n f = mod n f == 0

