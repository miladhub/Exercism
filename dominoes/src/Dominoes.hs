module Dominoes (chain) where

import Data.Tuple (swap)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain []    = Just []
chain (h:t) =
  let s = filter loops $ search h t
  in if null s then Nothing
  else Just (head s)

search :: (Int,Int) -> [(Int,Int)] -> [[(Int,Int)]]
search d []  = [[d]]
search d bag =
  map (d:) $ concat [ search candidate (bag `minus` i)
                    | i <- [0..(length bag - 1)],
                      let d' = bag !! i
                    , candidate <- [d', swap d'] 
                    , conn d candidate
                    ]

minus :: [a] -> Int -> [a]
minus b i =
  let s = splitAt i b
  in fst s ++ tail (snd s)

loops :: [(Int, Int)] -> Bool
loops l = conn (last l) (head l)

conn :: (Int, Int) -> (Int, Int) -> Bool
conn f s  = snd f == fst s

