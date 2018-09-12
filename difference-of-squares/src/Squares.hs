module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n = gaussSumUntil n ^ 2

gaussSumUntil :: Integral a => a -> a
gaussSumUntil n = (n + 1) * n `quot` 2

sumOfSquares :: Integral a => a -> a
sumOfSquares n = sum $ (^2) <$> [1..n]

