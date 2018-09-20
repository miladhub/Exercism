module DNA (nucleotideCounts) where

import Data.Map --(Map)
import Data.List

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs =
  let maybe = check xs
  in case maybe of
    Nothing -> Left xs
    (Just s) -> Right $ toMap s

toMap :: String -> Map Char Int
toMap s = fromList $ countList s

countList :: String -> [(Char, Int)]
countList xs =
  let g = group xs
  in zip (fmap head g) (fmap length g) 

check :: String -> Maybe String
check xs = sequence $ fmap foo xs

foo :: Char -> Maybe Char
foo c =
      if elem c "ACGT"
      then Just c
      else Nothing
