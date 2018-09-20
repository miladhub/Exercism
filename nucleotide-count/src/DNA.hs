module DNA (nucleotideCounts) where

import Data.Map (Map, fromList)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs =
  let checked = checkOnlyNucleotides xs
  in case checked of
    Nothing -> Left xs
    (Just xs) -> Right $ countChars xs

countChars :: String -> Map Char Int
countChars xs = fromList $ fmap (\c -> (c, length $ filter (== c) xs)) "ACGT"

checkOnlyNucleotides :: String -> Maybe String
checkOnlyNucleotides xs = sequence $ fmap pickNucleotide xs

pickNucleotide :: Char -> Maybe Char
pickNucleotide c =
      if elem c "ACGT"
      then Just c
      else Nothing
