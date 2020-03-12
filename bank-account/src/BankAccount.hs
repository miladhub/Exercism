module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Monad.STM (atomically)
import Control.Concurrent.STM (TVar, newTVar, readTVar, writeTVar)

type BankAccount = TVar (Maybe Integer)

closeAccount :: BankAccount -> IO ()
closeAccount account = atomically $
  writeTVar account Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = atomically $
  readTVar account

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = atomically $ do
  acc <- readTVar account
  let new = (+ amount) <$> acc
  writeTVar account new
  return new  

openAccount :: IO BankAccount
openAccount = atomically $ newTVar (Just 0)  
