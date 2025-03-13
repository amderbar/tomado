{-# LANGUAGE OverloadedStrings #-}

module TodoReadWriteSpec (spec) where

import Capability.TodoReadWritable
import Data.TodoEntity (TodoEntityT (..), concreteTodoEntity, emptyTodoEntity)
import Database.Model.Event (nullEventId)
import Database.Setup (initDb)
import Database.Util (Connection, withConnection)
import Import (void)
import Test.Hspec
import Tomado

setupDb :: (Connection -> IO ()) -> IO ()
setupDb action = withConnection ":memory:" (\conn -> initDb conn >> action conn)

spec :: Spec
spec = around setupDb $ do
  describe "Todo Readable and Writable Capability" $ do
    it "has no To-Do" $ \conn -> runAppM conn listTodoEntries `shouldReturn` []
    it "can create a To-Do" $ \conn -> do
      let todo = emptyTodoEntity {todoDescription = "Buy milk"}
      (todoId, createdAt) <- runAppM conn (createTodoEntry todo nullEventId)
      runAppM conn listTodoEntries `shouldReturn` [concreteTodoEntity todoId createdAt todo]
    it "can read a To-Do" $ \conn -> do
      let todo = emptyTodoEntity {todoDescription = "Buy milk"}
      (todoId, createdAt) <- runAppM conn (createTodoEntry todo nullEventId)
      runAppM conn (readTodoEntry todoId) `shouldReturn` Just (concreteTodoEntity todoId createdAt todo)
    it "can update a To-Do" $ \conn -> do
      let todo = emptyTodoEntity {todoDescription = "Buy milk"}
      (todoId, createdAt) <- runAppM conn (createTodoEntry todo nullEventId)
      let created = concreteTodoEntity todoId createdAt todo
      let updated = created {todoDescription = "Buy milk and eggs"}
      updatedAt <- runAppM conn (updateTodoEntry updated nullEventId)
      runAppM conn (readTodoEntry todoId) `shouldReturn` Just updated {todoUpdatedAt = Just updatedAt}
    it "can delete a To-Do" $ \conn -> do
      pendingWith "deleteTodoEntry function is not Implemented yet"
      let todo = emptyTodoEntity {todoDescription = "Buy milk"}
      (todoId, _) <- runAppM conn (createTodoEntry todo nullEventId)
      void $ runAppM conn (deleteTodoEntry todoId nullEventId)
      runAppM conn listTodoEntries `shouldReturn` []
