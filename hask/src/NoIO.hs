{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language ConstraintKinds #-}
module NoIO where

import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Map as M

data Options

concreteUserIncrement :: StateT Int IO Int
concreteUserIncrement = do
  incAmount <- liftIO readLn
  modify (+incAmount)
  return incAmount

classUserIncrement :: (MonadState Int m, MonadIO m) => m Int
classUserIncrement = do
  incAmount <- liftIO readLn
  modify (+incAmount)
  return incAmount

concreteReset :: ReaderT Options (StateT Int IO) ()
concreteReset = put 0

classbasedReset :: MonadState Int m => m ()
classbasedReset = put 0

class MonadFiles m where
  readAFile :: FilePath -> m String
  writeAFile :: FilePath -> String -> m ()

instance MonadFiles IO where
  readAFile = readFile
  writeAFile = writeFile

getDiary :: MonadFiles m => m String
getDiary = readAFile "my-diary.txt"

m :: StateT Int IO String
m = lift getDiary

instance MonadFiles (State (M.Map String String)) where
  readAFile fileName = do
    files <- get
    let contents = fromMaybe "" (M.lookup fileName files)
    return contents

  writeAFile fileName contents = do
    modify (M.insert fileName contents)

class MonadFileReader m where
  readAFile' :: FilePath -> m String

class MonadFileWriter m where
  writeAFile' :: FilePath -> String -> m ()

type MonadFiles' m = (MonadFileReader m, MonadFileWriter m)

