module Grains (square, total) where

import Control.Monad

square :: Integer -> Maybe Integer
square n | n > 0 && n <= 64 = Just . (2^) . (subtract 1) $ n
         | otherwise        = Nothing

total :: Integer
total = sum $ fmap (2^) . fmap (subtract 1) $ [1..64]

