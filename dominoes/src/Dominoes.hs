module Dominoes (chain) where

import Data.Tuple (swap)
import Data.List (permutations)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain d =
  let perms = permutations d >>= mapM (\e -> [e, swap e])
      matches = filter (\p -> connected p && looped p) perms
  in
    if null matches then
      Nothing
    else
      Just (head matches)

connected :: [(Int, Int)] -> Bool
connected (f : (s : t)) 
  | connects f s = connected (s : t)
  | otherwise    = False
connected _ = True

looped :: [(Int, Int)] -> Bool
looped []  = True
looped l   = connects (last l) (head l)

connects :: (Int, Int) -> (Int, Int) -> Bool
connects a b = snd a == fst b
