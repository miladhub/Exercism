module PigLatin (translate) where

import Data.Char (isLetter)

translate :: String -> String
translate xs =
  unwords $ fmap pigWord $ words xs

pigWord :: String -> String
pigWord xs =
  let v = pickFirstVowel xs
      c = pickFirstCons xs
  in
    case v of
      Just _ -> xs ++ "ay"
      Nothing ->
        case c of
          Just (h, t) -> t ++ h ++ "ay"
          Nothing -> ""

pickFirstVowel :: String -> Maybe (String, String)
pickFirstVowel xs
  | null xs = Nothing
  | isVowel (head xs) = Just ([head xs], tail xs)
  | startsWith "xr" xs = Just ("xr", tail . tail $ xs)
  | startsWith "yt" xs = Just ("yt", tail . tail $ xs)
  | otherwise = Nothing

pickFirstCons :: String -> Maybe (String, String)
pickFirstCons xs =
  let cons = takeWhile (\c -> isCons c && c /= 'y') xs
      rest = subStr (length cons) xs 
  in
    if (startsWith "qu" xs) then Just ("qu", subStr 2 xs)
    else
      if (startsWith "y" xs) then Just ("y", tail xs)
      else
        if (null cons) then Nothing
        else
          if (last cons == 'q' && startsWith "u" rest) then
            Just (cons ++ "u", subStr (length $ cons ++ "u") xs)
          else Just (cons, rest) 

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

isCons :: Char -> Bool
isCons c = isLetter c && not (isVowel c)

startsWith :: String -> String -> Bool
startsWith prefix xs = length prefix == (length $ filter (== True) $ zipWith (==) prefix xs)

subStr :: Int -> String -> String
subStr off xs = fmap (\i -> xs !! i) [off..length xs - 1] 
