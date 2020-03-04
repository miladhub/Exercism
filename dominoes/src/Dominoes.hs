module Dominoes (chain) where

import Data.Tuple (swap)
import Data.List (permutations)
import Data.Foldable (asum)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain d =
  let ps        = permutations d >>= mapM (\e -> [e, swap e])
      connected = fmap connect ps
      looping   = filter (maybe False loops) connected
  in asum looping

connect :: [(Int, Int)] -> Maybe [(Int, Int)]                                              
connect (h : (th : tt)) 
  | connects h th   = Just (h:)  <*> connect (th : tt)
  | otherwise       = Nothing
connect l = Just l

loops :: [(Int, Int)] -> Bool
loops []  = True
loops l   = connects (last l) (head l)

connects :: (Int, Int) -> (Int, Int) -> Bool
connects a b = snd a == fst b
