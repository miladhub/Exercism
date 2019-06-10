module DNA (nucleotideCounts) where

import Data.Map (Map, fromList, mapKeys, fromListWith)
import Text.Read
import Data.Bifunctor

data Nucleotide = A | C | G | T
  deriving (Eq, Ord, Read, Show)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs =
  let nucleotides = fmap countNucleotides $ checkOnlyNucleotides xs 
  in fmap (mapKeys asChar) nucleotides

asChar :: Nucleotide -> Char
asChar n =
  let s = show n
  in s !! 0

countNucleotides :: [Nucleotide] -> Map Nucleotide Int
countNucleotides ns =
  let zeroes = (,) <$> [A,G,C,T] <*> [0]
      ones   = (,) <$> ns        <*> [1]
  in fromListWith (+) $ zeroes ++ ones

checkOnlyNucleotides :: String -> Either String [Nucleotide]
checkOnlyNucleotides xs = traverse pickNucleotide xs

pickNucleotide :: Char -> Either String Nucleotide
pickNucleotide c =
  readEither [c]
