module Database.Util
  ( HasConnection (..),
    createTodoEntry,
    updateTodoEntry,
    getTodoEntry,
    getAllTodoEntries,
    module Database.SQLite.Simple,
  )
where

import Data.TodoEntity (NewTodoEntity, TodoEntity, TodoEntityT (todoDescription))
import Database.Beam
import Database.Beam.Sqlite
import Database.Command (createEvent, createTodo, createTodoCreated, createTodoUpdated)
import Database.Model.Event (Event, EventParentId, EventT (_eventOccurredAt))
import Database.Model.Event.TodoCreated (TodoCreated)
import Database.Model.Event.TodoUpdated (TodoUpdated)
import Database.Model.Todo (TodoId)
import Database.Query (getAllLatestTodo, getLatestTodo)
import Database.SQLite.Simple (Connection, withConnection)
import Import (Lens', lens)
import RIO.Time (LocalTime)

class HasConnection a where
  connectionL :: Lens' a Connection

instance HasConnection Connection where
  connectionL = lens id (\_ y -> y)

createTodoEntry :: NewTodoEntity -> EventParentId -> Connection -> IO (TodoId, LocalTime)
createTodoEntry newTodo parentEventId conn = runBeamSqlite conn $ do
  let todoDesc = todoDescription newTodo
  [ntd] <- runInsertReturningList createTodo
  [ev] <- runInsertReturningList (createEvent parentEventId)
  createTodoCreated todoDesc ntd ev
  pure (primaryKey ntd, _eventOccurredAt ev)

updateTodoEntry :: TodoEntity -> EventParentId -> Connection -> IO LocalTime
updateTodoEntry upd parentEventId conn = runBeamSqlite conn $ do
  [ev] <- runInsertReturningList (createEvent parentEventId)
  createTodoUpdated upd ev
  pure (_eventOccurredAt ev)

getTodoEntry :: Int -> Connection -> IO (Maybe (TodoCreated, Event, Maybe TodoUpdated, Maybe Event))
getTodoEntry tid conn = runBeamSqlite conn $ getLatestTodo (fromIntegral tid)

getAllTodoEntries :: Connection -> IO [(TodoCreated, Event, Maybe TodoUpdated, Maybe Event)]
getAllTodoEntries conn = runBeamSqlite conn getAllLatestTodo
