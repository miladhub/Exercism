module DNA (toRNA) where

import Control.Applicative

toRNA :: String -> Maybe String
toRNA xs = foldl connect (Just "") $ fmap compl xs

connect :: Maybe String -> Maybe Char -> Maybe String
connect s c = liftA2 (flip append) s c

append :: Char -> String -> String
append c = reverse . (:) c . reverse

compl :: Char -> Maybe Char
compl 'G' = Just 'C'
compl 'C' = Just 'G'
compl 'T' = Just 'A'
compl 'A' = Just 'U'
compl _   = Nothing
