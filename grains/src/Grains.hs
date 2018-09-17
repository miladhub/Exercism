module Grains (square, total) where

square :: Integer -> Maybe Integer
square n | n > 0 && n <= 64 = Just $ 2 ^ (n - 1)
         | otherwise        = Nothing

total :: Integer
total =
  let squares = sequence $ square <$> [1..64]
  in maybe 0 sum squares
