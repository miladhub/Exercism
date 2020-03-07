module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Monad.STM (atomically)
import Control.Concurrent.STM (TVar, newTVar, readTVar, writeTVar)

data Account = Account
  {
    balance :: Integer
  , open :: Bool
  }

type BankAccount = TVar Account

closeAccount :: BankAccount -> IO ()
closeAccount account = atomically $ do
  acc <- readTVar account
  writeTVar account (acc {open = False})

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = atomically $ do
  acc <- readTVar account
  return $
    if open acc then Just (balance acc)
    else Nothing

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = atomically $ do
  acc <- readTVar account
  if open acc then
    let new = amount + balance acc
    in do
      writeTVar account (acc {balance = new})
      return (Just new)
  else
    return Nothing

openAccount :: IO BankAccount
openAccount = atomically (newTVar $ Account 0 True)

