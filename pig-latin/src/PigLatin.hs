module PigLatin (translate) where

import Data.Char (isLetter)

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
  | startsWith "xr" xs = True
  | startsWith "yt" xs = True
  | otherwise = False

pickFirstCons :: String -> (String, String)
pickFirstCons xs
  | startsWith "qu" xs = split "qu" xs
  | startsWith "y" xs = split "y" xs
  | last cons == 'q' && startsWith "u" rest = split (cons ++ "u") xs
  | otherwise = split cons xs
  where
    cons = takeWhile (\c -> isCons c && c /= 'y') xs
    rest = subStr (length cons) xs 

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

isCons :: Char -> Bool
isCons c = isLetter c && not (isVowel c)

startsWith :: String -> String -> Bool
startsWith prefix xs = length prefix == (length $ filter (== True) $ zipWith (==) prefix xs)

split :: String -> String -> (String, String)
split prefix xs = (prefix, subStr (length prefix) xs)

subStr :: Int -> String -> String
subStr off xs = fmap (\i -> xs !! i) [off..length xs - 1] 
