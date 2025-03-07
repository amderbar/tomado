{-# LANGUAGE FlexibleInstances #-}
module Test.TaskRepositoryDb where

import Import
import Capability.TaskRepository
import Database.Util
import Database.SQLite.Simple (Connection)

type TaskRepositoryDb = ReaderT Connection IO

instance TaskRepository TaskRepositoryDb where
  createTask t = ask >>= (liftIO . insertTask t)
  readTask i = ask >>= (liftIO . findTask i)
  updateTask = createTask
  deleteTask i = ask >>= (\conn -> liftIO $ undefined)
  listTasks = ask >>= (liftIO . allTasks)

runTaskRepositoryDb :: Connection -> TaskRepositoryDb a -> IO a
runTaskRepositoryDb conn dsl = runReaderT dsl conn
