module ArmstrongNumbers (armstrong) where

armstrong :: (Integral a, Show a, Read a) => a -> Bool
armstrong a =
  let n = length . digits $ a
  in (== a) . sum . fmap (^n) . digits $ a

digits :: (Integral a, Show a, Read a) => a -> [a]
digits = fmap (read . (:[])) . show
