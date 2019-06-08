module DNA (nucleotideCounts) where

import Data.Map (Map, fromList)
import Text.Read
import Data.Bifunctor

data Nucleotide = A | C | G | T
  deriving (Eq, Ord, Read, Show)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs =
  let nucleotides = fmap countNucleotides $ checkOnlyNucleotides xs 
      chars = (fmap . fmap) (first asChar) nucleotides
  in fmap fromList chars

asChar :: Nucleotide -> Char
asChar n =
  let s = show n
  in s !! 0

countNucleotides :: [Nucleotide] -> [(Nucleotide, Int)]
countNucleotides ns = fmap (\n -> (n, length $ filter (== n) ns)) [A,C,G,T]

checkOnlyNucleotides :: String -> Either String [Nucleotide]
checkOnlyNucleotides xs = sequence $ fmap pickNucleotide xs

pickNucleotide :: Char -> Either String Nucleotide
pickNucleotide c =
  readEither [c]
