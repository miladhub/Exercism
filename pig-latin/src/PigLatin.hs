module PigLatin (translate) where

import Data.Char (isLetter)
import Data.List (isPrefixOf)

translate :: String -> String
translate =
  unwords . fmap pigWord . words

pigWord :: String -> String
pigWord xs
  | null xs = ""
  | startsWithVowel xs = xs ++ "ay"
  | otherwise = t ++ cons ++ "ay"
  where
    (cons, t) = pickFirstCons xs

startsWithVowel :: String -> Bool
startsWithVowel xs
  | isVowel (head xs) = True
  | "xr" `isPrefixOf` xs = True
  | "yt" `isPrefixOf` xs = True
  | otherwise = False

pickFirstCons :: String -> (String, String)
pickFirstCons xs
  | "qu" `isPrefixOf` xs = split "qu" xs
  | "y" `isPrefixOf` xs = split "y" xs
  | last cons == 'q' && "u" `isPrefixOf` rest = split (cons ++ "u") xs
  | otherwise = split cons xs
  where
    cons = takeWhile (\c -> isCons c && c /= 'y') xs
    rest = drop (length cons) xs 

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

isCons :: Char -> Bool
isCons c = isLetter c && not (isVowel c)

split :: String -> String -> (String, String)
split prefix xs = (prefix, drop (length prefix) xs)
