module Connect (Mark(..), winner) where

import Control.Applicative

data Mark =
  Cross | Nought
  deriving (Eq, Show)

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
find board checkedCells char cell
  | outOfBounds board row col || alreadyChecked cell checkedCells || noMatch board row col char = Nothing
  | char == 'X' && col == lastCol board = Just Cross
  | char == 'O' && row == lastRow board = Just Nought
  | otherwise =
    foldl (<|>) Nothing $
      (find board (cell : checkedCells) char) <$>
        [(row - 1, col), (row + 1, col), (row, col + 1), (row, col - 1), (row + 1, col - 1), (row - 1, col + 1)]
  where
    (row, col) = cell
    outOfBounds board row col = row > lastRow board || col > lastCol board || row < 0 || col < 0
    alreadyChecked = elem
    noMatch board row col = (/=) (board !! row !! col)
 
lastCol :: [String] -> Int
lastCol board = (length $ board !! 0) - 1

lastRow :: [String] -> Int
lastRow board = (length board) - 1

