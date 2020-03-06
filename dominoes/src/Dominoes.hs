module Dominoes (chain) where

import Data.Tuple (swap)
import Safe (headMay)
import Data.List (delete)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain []    = Just []
chain (h:t) =
  headMay $ filter loops $ search h t

search :: (Int,Int) -> [(Int,Int)] -> [[(Int,Int)]]
search d []  = [[d]]
search d bag =
  map (d:) $ concat [ search candidate (delete d' bag)
                    | d' <- bag
                    , candidate <- [d', swap d'] 
                    , conn d candidate
                    ]

loops :: [(Int, Int)] -> Bool
loops l = conn (last l) (head l)

conn :: (Int, Int) -> (Int, Int) -> Bool
conn f s  = snd f == fst s

