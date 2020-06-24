module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO, replicateM)
import System.Random (getStdRandom, randomR)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, writeTVar)

type Robot = TVar String
type RunState = [String]

initialState :: RunState
initialState = []

mkRobot :: StateT RunState IO Robot
mkRobot = do
  ns <- get
  n <- liftIO $ uniqueName ns
  put (n:ns)
  liftIO $ newTVarIO n

resetName :: Robot -> StateT RunState IO ()
resetName r = do
  ns <- get
  n <- liftIO $ readTVarIO r
  put (n:ns)
  n' <- liftIO $ uniqueName (n:ns)
  liftIO $ atomically (writeTVar r n')
  return ()

robotName :: Robot -> IO String
robotName = readTVarIO

uniqueName :: [String] -> IO String
uniqueName ns = do
  n <- rndName
  if (elem n ns) then uniqueName ns
  else return n

rndName :: IO String
rndName = (++) <$> replicateM 2 rndChar <*> replicateM 3 rndDigit

rndChar :: IO Char
rndChar = getStdRandom $ randomR ('A', 'Z')

rndDigit :: IO Char
rndDigit = getStdRandom $ randomR ('0', '9')
