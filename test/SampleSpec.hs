{-# LANGUAGE OverloadedStrings #-}
module SampleSpec (spec) where

import Data.TomadoTask
import Capability.TaskRepository
import Test.TaskRepositoryDb
import Test.Hspec
import Database.Util (withTomadoDb, initDb)
import Database.SQLite.Simple (Connection)

setupDb :: (Connection -> IO ()) -> IO ()
setupDb action = withTomadoDb ":memory:" (\conn -> initDb conn >> action conn)

spec :: Spec
spec = around setupDb $ do
  describe "TaskRepository" $ do
    it "has no task" $ \conn -> runTaskRepositoryDb conn listTasks `shouldReturn` []
    it "can create a task" $ \conn -> do
      let task = TomadoTask (TomadoTaskId "1") "task1" "description1"
      runTaskRepositoryDb conn (createTask task)
      runTaskRepositoryDb conn listTasks `shouldReturn` [task]
    it "can read a task" $ \conn -> do
      let targetId = TomadoTaskId "2"
      let task = TomadoTask targetId "task2" "description2"
      runTaskRepositoryDb conn (createTask task)
      runTaskRepositoryDb conn (readTask targetId) `shouldReturn` Just task
    it "can update a task" $ \conn -> do
      let targetId = TomadoTaskId "3"
      let task = TomadoTask targetId "task3" "description3"
      runTaskRepositoryDb conn (createTask task)
      let updatedTask = TomadoTask targetId "task3'" "description3'"
      runTaskRepositoryDb conn (updateTask updatedTask)
      runTaskRepositoryDb conn (readTask targetId) `shouldReturn` Just updatedTask
    it "can delete a task" $ \conn -> do
      let targetId = TomadoTaskId "4"
      let task = TomadoTask targetId "task4" "description4"
      runTaskRepositoryDb conn (createTask task)
      runTaskRepositoryDb conn (deleteTask targetId)
      runTaskRepositoryDb conn (readTask targetId) `shouldReturn` Nothing
