module Database.Util
  ( insertTask,
    findTask,
    allTasks,
    createTodoEntry,
    updateTodoEntry,
    getAllTodoEntries,
    module Database.SQLite.Simple,
  )
where

import Data.TodoEntity (NewTodoEntity, TodoEntity, TodoEntityT (..))
import Data.TomadoTask
import Database.Beam
import Database.Beam.Sqlite
import Database.Command (createEvent, createTodo, createTodoCreated, createTodoUpdated)
import Database.Model.Event (Event, EventParentId)
import Database.Model.Event.TodoCreated (TodoCreated)
import Database.Model.Event.TodoUpdated (TodoUpdated)
import Database.Query (allTodoEntries)
import Database.SQLite.Simple (Connection, withConnection)
import Database.Schema

insertTask :: TomadoTask -> Connection -> IO ()
insertTask (TomadoTask (TomadoTaskId i) n d) conn =
  runBeamSqlite conn $
    runInsert $
      insert (_tomadoDbTasks tomadoDb) $
        insertExpressions [Task default_ (val_ i) (val_ n) (val_ d) currentTimestamp_]

findTask :: TomadoTaskId -> Connection -> IO (Maybe TomadoTask)
findTask (TomadoTaskId i) conn = runBeamSqlite conn $ do
  tasks <- runSelectReturningOne $ select $ filter_ (\t -> _taskNum t ==. val_ i) $ all_ (_tomadoDbTasks tomadoDb)
  return $ fmap toTomadoTask tasks

allTasks :: Connection -> IO [TomadoTask]
allTasks conn = runBeamSqlite conn $ do
  tasks <- runSelectReturningList $ select $ all_ (_tomadoDbTasks tomadoDb)
  return $ fmap toTomadoTask tasks

createTodoEntry :: NewTodoEntity -> EventParentId -> Connection -> IO ()
createTodoEntry newTodo parentEventId conn = runBeamSqlite conn $ do
  let todoDesc = todoDescription newTodo
  [ntd] <- runInsertReturningList createTodo
  [ev] <- runInsertReturningList (createEvent parentEventId)
  createTodoCreated todoDesc ntd ev

updateTodoEntry :: TodoEntity -> EventParentId -> Connection -> IO ()
updateTodoEntry upd parentEventId conn = runBeamSqlite conn $ do
  [ev] <- runInsertReturningList (createEvent parentEventId)
  createTodoUpdated upd ev

getAllTodoEntries :: Connection -> IO [(TodoCreated, Event, Maybe TodoUpdated, Maybe Event)]
getAllTodoEntries conn = runBeamSqlite conn allTodoEntries
