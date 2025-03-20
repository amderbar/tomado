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
    context "with an empty database" $ do
      it "has no To-Do" $ \conn -> runAppM listTodoEntries conn `shouldReturn` []
    it "can create a To-Do" $ \conn -> do
      let todo = emptyTodoEntity {todoDescription = "Buy milk", todoPriority = 1}
      (todoId, createdAt, updatedAt) <- runAppM (createTodoEntry todo nullEventId) conn
      let expected = concreteTodoEntity todoId createdAt todo {todoUpdatedAt = updatedAt}
      runAppM listTodoEntries conn `shouldReturn` [expected]
    it "can read a To-Do" $ \conn -> do
      let todo = emptyTodoEntity {todoDescription = "Buy milk"}
      (todoId, createdAt, updatedAt) <- runAppM (createTodoEntry todo nullEventId) conn
      let expected = concreteTodoEntity todoId createdAt todo {todoUpdatedAt = updatedAt}
      runAppM (readTodoEntry todoId) conn `shouldReturn` Just expected
    it "can update a To-Do" $ \conn -> do
      let todo = emptyTodoEntity {todoDescription = "Buy milk"}
      (todoId, createdAt, _) <- runAppM (createTodoEntry todo nullEventId) conn
      let created = concreteTodoEntity todoId createdAt todo
      let updated = created {todoDescription = "Buy milk and eggs"}
      updatedAt <- runAppM (updateTodoEntry updated nullEventId) conn
      runAppM (readTodoEntry todoId) conn `shouldReturn` Just updated {todoUpdatedAt = Just updatedAt}
    it "can delete a To-Do" $ \conn -> do
      pendingWith "deleteTodoEntry function is not Implemented yet"
      let todo = emptyTodoEntity {todoDescription = "Buy milk"}
      (todoId, _, _) <- runAppM (createTodoEntry todo nullEventId) conn
      void $ runAppM (deleteTodoEntry todoId nullEventId) conn
      runAppM listTodoEntries conn `shouldReturn` []
