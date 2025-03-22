module Database.Util
  ( HasConnection (..),
    createTodoEntry,
    updateTodoEntry,
    getTodoEntry,
    getAllTodoEntries,
    throwAwayTodoEntry,
    module Database.Beam,
    module Database.SQLite.Simple,
  )
where

import Data.TodoEntity (TodoEntity)
import qualified Data.TodoEntity as TE (TodoId)
import Database.Beam (SqlJustable (nothing_), Table (primaryKey))
import Database.Beam.Sqlite (runBeamSqlite, runInsertReturningList)
import Database.Command (createEvent, createTodo, createTodoCreated, createTodoTrashed, createTodoUpdated)
import Database.Model.Event (Event, EventParentId)
import Database.Model.Event.TodoCreated (TodoCreated)
import Database.Model.Event.TodoUpdated (TodoUpdated)
import Database.Model.Todo (TodoId)
import Database.Query (getAllLatestTodo, getLatestTodo)
import Database.SQLite.Simple (Connection, withConnection)
import Import (Lens', Text, lens)

class HasConnection a where
  connectionL :: Lens' a Connection

instance HasConnection Connection where
  connectionL = lens id (\_ y -> y)

createTodoEntry :: Text -> EventParentId -> Connection -> IO (TodoId, Event)
createTodoEntry todoDesc parentEventId conn = runBeamSqlite conn $ do
  [ntd] <- runInsertReturningList createTodo
  [ev] <- runInsertReturningList (createEvent parentEventId)
  createTodoCreated todoDesc ntd ev
  pure (primaryKey ntd, ev)

updateTodoEntry :: TodoEntity -> EventParentId -> Connection -> IO Event
updateTodoEntry upd parentEventId conn = runBeamSqlite conn $ do
  [ev] <- runInsertReturningList (createEvent parentEventId)
  createTodoUpdated upd ev
  pure ev

throwAwayTodoEntry :: TE.TodoId -> Bool -> EventParentId -> Connection -> IO Event
throwAwayTodoEntry tid isTrashed parentEventId conn = runBeamSqlite conn $ do
  [ev] <- runInsertReturningList (createEvent parentEventId)
  createTodoTrashed tid isTrashed ev
  pure ev

getTodoEntry :: Int -> Connection -> IO (Maybe (TodoCreated, Event, Maybe TodoUpdated, Maybe Event))
getTodoEntry tid conn = runBeamSqlite conn $ getLatestTodo (fromIntegral tid)

getAllTodoEntries :: Connection -> IO [(TodoCreated, Event, Maybe TodoUpdated, Maybe Event)]
getAllTodoEntries conn = runBeamSqlite conn getAllLatestTodo
