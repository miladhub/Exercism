module DNA (toRNA) where

import Control.Applicative

toRNA :: String -> Maybe String
toRNA xs = reverse <$> (foldr connect (Just "") $ fmap compl xs)

connect :: Maybe Char -> Maybe String -> Maybe String
connect xs c = append <$> xs <*> c

append :: Char -> String -> String
append c xs = xs ++ [c]

compl :: Char -> Maybe Char
compl 'G' = Just 'C'
compl 'C' = Just 'G'
compl 'T' = Just 'A'
compl 'A' = Just 'U'
compl _   = Nothing
