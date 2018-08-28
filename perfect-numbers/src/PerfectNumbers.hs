module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n <= 0             = Nothing
    | sumOfFactors n > n = Just Abundant
    | sumOfFactors n < n = Just Deficient
    | otherwise          = Just Perfect
    where
        sumOfFactors n = sum $ factors n

factors :: Int -> [Int]
factors n = filter (isFactor n) [1..(n - 1)]

isFactor :: Int -> Int -> Bool
isFactor n m = mod n m == 0

