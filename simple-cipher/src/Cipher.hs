module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr)
import System.Random (randomRs, newStdGen)

caesarDecode :: String -> String -> String
caesarDecode key encodedText =
  zipWith unshift (cycle key) encodedText

caesarEncode :: String -> String -> String
caesarEncode key text =
  zipWith shift (cycle key) text

shift :: Char -> Char -> Char
shift k c =
  let d = ord k - ord 'a'
      p = ord c - ord 'a'
  in chr $ (ord 'a' + (p + d) `modAbs` 26)

unshift :: Char -> Char -> Char
unshift k c =
  let d = ord k - ord 'a'
      p = ord c - ord 'a'
  in chr $ (ord 'a' + (p - d) `modAbs` 26)

modAbs :: Int -> Int -> Int
modAbs x m
  | x >= 0    = x `mod` m
  | otherwise = (m + x) `mod` m 

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- take 3 <$> randomRs ('a', 'z') <$> newStdGen
  return (key, caesarEncode key text)
