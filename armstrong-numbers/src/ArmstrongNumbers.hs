module ArmstrongNumbers (armstrong) where

import Data.Char

armstrong :: Int -> Bool
armstrong a =
  let asDigits = digits a
      n = length asDigits
  in (== a) . sum . fmap (^n) $ asDigits

digits :: Int -> [Int]
digits = fmap digitToInt . show
