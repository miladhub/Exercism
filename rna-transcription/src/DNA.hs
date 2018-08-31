module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA = sequence . fmap compl

compl :: Char -> Maybe Char
compl = flip lookup transcription

transcription :: [(Char,Char)]
transcription = [
    ('G', 'C'),
    ('C', 'G'),
    ('T', 'A'),
    ('A', 'U')
  ]
