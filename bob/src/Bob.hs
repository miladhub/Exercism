module Bob (responseFor) where

import Data.Char
import Control.Applicative

responseFor :: String -> String
responseFor s
  | yellq s      = "Calm down, I know what I'm doing!"
  | question s   = "Sure."
  | nosentence s = "Fine. Be that way!"
  | yell s       = "Whoa, chill out!"
  | otherwise    = "Whatever."

question = liftA2 (&&) (not . null . words) ((== '?') . last . last . words)

yellq = liftA2 (&&) question yell

yell = liftA2 (&&) (not . any isLower) (any isUpper)

nosentence = null . (filter $ any isAlphaNum) . words

