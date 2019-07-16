module Dominoes (chain) where

import Data.Tuple (swap)
import Data.List (permutations)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain d =
  let ps        = permutations d
      connected = fmap connect ps
      looping   = filter loops connected
  in case looping of
    [] -> Nothing
    _  -> head looping

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

loops :: Maybe [(Int, Int)] -> Bool
loops Nothing    = False
loops (Just [])  = True
loops (Just [a]) = fst a == snd a
loops (Just l)   = fst (head l) == snd (last l)
