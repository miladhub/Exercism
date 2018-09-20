module DNA (nucleotideCounts) where

import Data.Map (Map, fromList)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs = fmap countChars $ checkOnlyNucleotides xs 

countChars :: String -> Map Char Int
countChars xs = fromList $ fmap (\c -> (c, length $ filter (== c) xs)) "ACGT"

checkOnlyNucleotides :: String -> Either String String
checkOnlyNucleotides xs = sequence $ fmap pickNucleotide xs

pickNucleotide :: Char -> Either String Char
pickNucleotide c =
      if elem c "ACGT"
      then Right c
      else Left [c]
