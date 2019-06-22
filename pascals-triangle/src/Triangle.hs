module Triangle (rows) where

rows :: Int -> [[Int]]
rows 0 = []
rows 1 = [[1]]
rows n =
  let previous = rows $ n - 1
  in previous ++ [next (head $ reverse previous)]

next :: [Int] -> [Int]
next r =
  let padded = 0 : r ++ [0]
  in fmap (uncurry (+)) $ zip padded (drop 1 padded)
