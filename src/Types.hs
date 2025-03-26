{-# LANGUAGE NoImplicitPrelude #-}

module Types
  ( App (..),
    Options (..),
    Action (..),
    ConfigOpt (..),
    ListTodoOpt (..),
    ViewTodoOpt (..),
    AddTodoOpt (..),
    UpdateTodoOpt (..),
    TrashTodoOpt (..),
    WorkSpace (..),
    Config (..),
    getDbPath,
    emptyUpdateTodoOpt,
    HasConnection (..),
  )
where

import Database.SQLite.Simple (Connection)
import RIO
import RIO.FilePath ((</>))
import RIO.Process
import RIO.Time (LocalTime)

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool,
    optionsAction :: !Action
  }

data Action
  = Configure ConfigOpt
  | ListTodo ListTodoOpt
  | ViewTodo ViewTodoOpt
  | AddTodo AddTodoOpt
  | UpdateTodo UpdateTodoOpt
  | TrashTodo TrashTodoOpt

data ConfigOpt = ConfigOpt

data ListTodoOpt = ListTodoOpt

newtype ViewTodoOpt = ViewTodoOpt
  { viewTodoId :: Int
  }
  deriving (Show)

data AddTodoOpt = AddTodoOpt
  { addTodoDescription :: !Text,
    addTodoDetail :: !Bool,
    addTodoPriority :: !(Maybe Int),
    addTodoDueDate :: !(Maybe LocalTime)
  }
  deriving (Show)

data UpdateTodoOpt = UpdateTodoOpt
  { updateTodoId :: !Int,
    updateTodoDescription :: !(Maybe Text),
    updateTodoDetail :: !Bool,
    updateTodoPriority :: !(Maybe Int),
    updateTodoDueDate :: !(Maybe LocalTime),
    updateTodoDone :: !(Maybe Bool)
  }
  deriving (Show)

emptyUpdateTodoOpt :: Int -> UpdateTodoOpt
emptyUpdateTodoOpt i = UpdateTodoOpt i Nothing False Nothing Nothing Nothing

newtype TrashTodoOpt = TrashTodoOpt
  { trashTodoId :: Int
  }
  deriving (Show)

data WorkSpace = WorkSpace
  { wsRoot :: !FilePath,
    wsDbName :: !String
  }

getDbPath :: WorkSpace -> FilePath
getDbPath ws = wsRoot ws </> wsDbName ws

newtype Config = Config
  { configEditor :: String
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options,
    -- Add other app-specific configuration information here
    appConnection :: !Connection,
    appWorkSpace :: !WorkSpace,
    appConfig :: !Config
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

class HasConnection a where
  connectionL :: Lens' a Connection

instance HasConnection Connection where
  connectionL = lens id (\_ y -> y)

instance HasConnection App where
  connectionL = lens appConnection (\x y -> x {appConnection = y})
