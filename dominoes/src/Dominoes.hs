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
  | connects h th               = pure (:) <*> Just h        <*> connect t
  | connects (swap h) th        = pure (:) <*> Just (swap h) <*> connect t
  | connects h (swap th)        = pure (:) <*> Just h        <*> connect ((swap th) : tt)
  | connects (swap h) (swap th) = pure (:) <*> Just (swap h) <*> connect ((swap th) : tt)
  | otherwise                   = Nothing
  where
    connects (_, a) (b, _) = a == b

loops :: Maybe [(Int, Int)] -> Bool
loops Nothing    = False
loops (Just [])  = True
loops (Just [a]) = fst a == snd a
loops (Just l)   = fst (head l) == snd (last l)
