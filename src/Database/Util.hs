{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Util
  ( withTomadoDb,
    initDb,
    insertTask,
    findTask,
    allTasks,
  )
where

import Data.TomadoTask
import Database.Beam
import Database.Beam.Migrate.Generics
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate
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
