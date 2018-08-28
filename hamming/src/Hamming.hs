module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just $ length $ filter (\(f,s) -> f /= s) $ zip xs ys

