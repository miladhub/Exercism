module Triangle (rows) where

rows :: Int -> [[Int]]
rows n = take n $ iterate next [1]

next :: [Int] -> [Int]
next r =
  let padded = 0 : r ++ [0]
      pairs = zip padded $ drop 1 padded
  in fmap (uncurry (+)) pairs
