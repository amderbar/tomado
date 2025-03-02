{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Util where

import Data.TodoEntity (NewTodoEntity, TodoEntityT (todoDescription))
import Data.TomadoTask
import Database.Beam
import Database.Beam.Migrate.Generics
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate
import Database.Model.Event (Event, EventParentId, EventT (Event, _eventId))
import Database.Model.Event.TodoCreated (TodoCreated, TodoCreatedT (TodoCreated, _todoCreatedEvent, _todoCreatedTodo))
import Database.Model.Event.TodoUpdated (TodoUpdated, TodoUpdatedT (_todoUpdatedEvent, _todoUpdatedTodo))
import Database.Model.Todo (TodoT (Todo))
import Database.SQLite.Simple
import Database.Schema
import Import

checkedTomadoDb :: CheckedDatabaseSettings Sqlite TomadoDb
checkedTomadoDb = defaultMigratableDbSettings

withTomadoDb :: String -> (Connection -> IO a) -> IO a
withTomadoDb fname = bracket (open fname) close

initDb :: Connection -> IO ()
initDb = flip runBeamSqlite (autoMigrate migrationBackend checkedTomadoDb)

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
  [ntd] <-
    runInsertReturningList $
      insert (_tomadoDbTodo tomadoDb) $
        insertExpressions [Todo default_]
  [ev] <-
    runInsertReturningList $
      insert (_tomadoDbEvents tomadoDb) $
        insertExpressions [Event default_ (val_ parentEventId) currentTimestamp_]
  runInsert $
    insert (_tomadoDbTodoCreated tomadoDb) $
      insertExpressions [TodoCreated default_ (val_ $ primaryKey ev) (val_ $ primaryKey ntd) (val_ todoDesc)]

allTodoEntries :: Connection -> IO [(TodoCreated, Event, Maybe TodoUpdated, Maybe Event)]
allTodoEntries conn = runBeamSqliteDebug putStrLn conn $
  runSelectReturningList $ select $ do
    ev <- all_ (_tomadoDbEvents tomadoDb)
    crd <- oneToOne_ (_tomadoDbTodoCreated tomadoDb) _todoCreatedEvent ev
    (upd, updEv) <- leftJoin_ latestUpdateEvents (\(u, _) -> _todoUpdatedTodo u ==. _todoCreatedTodo crd)
    pure (crd, ev, upd, updEv)
  where
    latestUpdateEvents = do
      (_, eid) <- aggregate_ (\(u, e) -> (group_ $ _todoUpdatedTodo u, max_ (_eventId e))) $ do
        e <- all_ (_tomadoDbEvents tomadoDb)
        u <- oneToMany_ (_tomadoDbTodoUpdated tomadoDb) _todoUpdatedEvent e
        pure (u, e)
      e <- join_' (_tomadoDbEvents tomadoDb) (\e -> just_ (_eventId e) ==?. eid)
      upd <- oneToMany_ (_tomadoDbTodoUpdated tomadoDb) _todoUpdatedEvent e
      pure (upd, e)
