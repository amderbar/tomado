{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Tomado where

import Capability.TodoReadWritable
import Data.TodoEntity (NewTodoEntity, TodoEntity, TodoEntityT (..), TodoId (TodoId))
import Database.Model.Event (Event, EventParentId, EventT (_eventOccurredAt))
import Database.Model.Event.TodoCreated (TodoCreated, TodoCreatedT (_todoCreatedDescription, _todoCreatedId))
import Database.Model.Event.TodoUpdated (TodoUpdated, TodoUpdatedT (_todoUpdatedDescription, _todoUpdatedDetail, _todoUpdatedDone, _todoUpdatedDueDate, _todoUpdatedPriority))
import qualified Database.Model.Todo as M (PrimaryKey (TodoId))
import Database.Query (TodoQueryReturn)
import Database.Util
  ( Connection,
    HasConnection (connectionL),
    getAllTodoEntries,
    getTodoEntry,
  )
import qualified Database.Util as DU (createTodoEntry, updateTodoEntry)
import Import
import RIO.Time (LocalTime)

newtype AppM env a = AppM {unAppM :: RIO env a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader env)

runAppM :: env -> AppM env a -> IO a
runAppM env = runRIO env . unAppM

instance (HasConnection env) => TodoReadable (AppM env) where
  readTodoEntry :: TodoId -> AppM env (Maybe TodoEntity)
  readTodoEntry (TodoId i) = runQuery (getTodoEntry i)

  listTodoEntries :: AppM env [TodoEntity]
  listTodoEntries = runQuery getAllTodoEntries

instance (HasConnection env) => TodoWritable (AppM env) where
  createTodoEntry :: NewTodoEntity -> EventParentId -> AppM env (TodoId, LocalTime)
  createTodoEntry td eid = do
    conn <- asks (view connectionL)
    (M.TodoId i, createdAt) <- liftIO $ DU.createTodoEntry td eid conn
    pure (TodoId (fromIntegral i), createdAt)

  updateTodoEntry :: TodoEntity -> EventParentId -> AppM env LocalTime
  updateTodoEntry td eid = do
    conn <- asks (view connectionL)
    liftIO $ DU.updateTodoEntry td eid conn

  deleteTodoEntry :: TodoId -> EventParentId -> AppM env LocalTime
  deleteTodoEntry = undefined

runQuery :: (HasConnection env, Functor m) => (Connection -> IO (m TodoQueryReturn)) -> AppM env (m TodoEntity)
runQuery query = do
  conn <- asks (view connectionL)
  ret <- (liftIO . query) conn
  pure $ replayTodoEvents <$> ret

replayTodoEvents :: (TodoCreated, Event, Maybe TodoUpdated, Maybe Event) -> TodoEntity
replayTodoEvents (c, ce, mu, mue) =
  TodoEntity
    { _todoId = Identity $ TodoId (fromIntegral $ _todoCreatedId c),
      todoDescription = maybe (_todoCreatedDescription c) _todoUpdatedDescription mu,
      todoDetail = maybe "" _todoUpdatedDetail mu,
      todoDone = maybe False _todoUpdatedDone mu,
      _todoCreatedAt = Identity $ _eventOccurredAt ce,
      todoUpdatedAt = _eventOccurredAt <$> mue,
      todoPriority = maybe 0 (fromIntegral . _todoUpdatedPriority) mu,
      todoDueDate = _todoUpdatedDueDate =<< mu,
      todoContext = Nothing, -- TODO: _todoUpdatedContext =<< mu,
      todoProject = Nothing, -- TODO: _todoUpdatedProject =<< mu,
      todoTags = [],
      todoParent = Nothing, -- TODO: _todoUpdatedParent =<< mu,
      todoComments = []
    }
