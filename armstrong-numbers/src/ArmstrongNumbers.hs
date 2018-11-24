module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong a =
  let n = length . digits $ a
  in (== a) . sum . fmap (^n) . digits $ a

digits :: Integral a => a -> [a]
digits a =
  let first = a `mod` 10
  in
    if first == a then
      [first]
    else
      digits (a `div` 10) ++ [first]
