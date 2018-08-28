module Connect (Mark(..), winner) where

import Control.Applicative

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board =
  foldl (<|>) Nothing $ (spot board) <$> [Cross, Nought] 

spot :: [String] -> Mark -> Maybe Mark
spot board mark =
  foldl (<|>) Nothing $ pathsFromStart board mark

pathsFromStart :: [String] -> Mark -> [Maybe Mark]
pathsFromStart board Cross  = [find board [] 'X' (r, 0) | r <- [0..(lastRow board)]]
pathsFromStart board Nought = [find board [] 'O' (0, c) | c <- [0..(lastCol board)]]

find :: [String] -> [(Int,Int)] -> Char -> (Int, Int) -> Maybe Mark
find b e ch rc =
  let (r,c) = rc
      outOfBounds b r c = r > lastRow b || c > lastCol b || r < 0 || c < 0
      alreadyChecked = elem
      noMatch b r c = (/=) (b !! r !! c)
  in
    if outOfBounds b r c || alreadyChecked rc e || noMatch b r c ch then Nothing
    else if ch == 'X' && c == lastCol b then Just Cross
    else if ch == 'O' && r == lastRow b then Just Nought
    else
      foldl (<|>) Nothing $
        (find b (rc : e) ch) <$>
          [(r - 1, c), (r + 1, c), (r, c + 1), (r, c - 1), (r + 1, c - 1), (r - 1, c + 1)]

lastCol :: [String] -> Int
lastCol board = (length $ board !! 0) - 1

lastRow :: [String] -> Int
lastRow board = (length board) - 1

