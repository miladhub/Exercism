module Dominoes (chain) where

import Data.Tuple (swap)
import Data.List (permutations)
import Data.Foldable (asum)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain d =
  let ps        = permutations d
      connected = fmap connect ps
      looping   = filter (maybe False loops) connected
  in asum looping

connect :: [(Int, Int)] -> Maybe [(Int, Int)]                                              
connect []  = Just []
connect [s] = Just [s]
connect (h : t@(th : tt)) 
  | connects h th   = Just (h:)  <*> connect t
  | connects hs th  = Just (hs:) <*> connect t
  | connects h ths  = Just (h:)  <*> connect (ths : tt)
  | connects hs ths = Just (hs:) <*> connect (ths : tt)
  | otherwise       = Nothing
  where
    hs = swap h
    ths = swap th
    connects (_, a) (b, _) = a == b

loops :: [(Int, Int)] -> Bool
loops []  = True
loops [a] = fst a == snd a
loops l   = fst (head l) == snd (last l)
