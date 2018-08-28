module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
  | multipleOf 400 = True
  | multipleOf 100 = False
  | multipleOf 4   = True
  | otherwise      = False
  where
    multipleOf n = mod year n == 0



