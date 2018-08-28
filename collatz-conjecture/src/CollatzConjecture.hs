module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | otherwise = Just . toInteger . length . takeWhile (>1) $ steps n  

steps :: Integer -> [Integer]
steps n = scanl (flip const) n $ steps $ next n

next :: Integer -> Integer
next n
  | even n = div n 2
  | odd n = 3 * n + 1
