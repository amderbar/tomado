module Database.Setup
  ( initDb
  ) where

import Database.Beam.Migrate.Generics
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate
import Database.SQLite.Simple
import Database.Schema

checkedTomadoDb :: CheckedDatabaseSettings Sqlite TomadoDb
checkedTomadoDb = defaultMigratableDbSettings

initDb :: Connection -> IO ()
initDb = flip runBeamSqlite (autoMigrate migrationBackend checkedTomadoDb)
