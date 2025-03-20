{-# LANGUAGE NoImplicitPrelude #-}

module Types
  ( App (..),
    Options (..),
    Action (..),
    InitOpt (..),
    ConfigOpt (..),
    ListTodoOpt (..),
    AddTodoOpt (..),
    UpdateTodoOpt (..),
    WorkSpace (..),
    getDbPath,
    emptyUpdateTodoOpt,
  )
where

import RIO
import RIO.Process
import RIO.Time (LocalTime)

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool,
    optionsAction :: !Action
  }

data Action
  = Init InitOpt
  | Config ConfigOpt
  | ListTodo ListTodoOpt
  | AddTodo AddTodoOpt
  | UpdateTodo UpdateTodoOpt

data InitOpt = InitOpt

data ConfigOpt = ConfigOpt

data ListTodoOpt = ListTodoOpt

data AddTodoOpt = AddTodoOpt
  { addTodoDescription :: !Text,
    addTodoPriority :: !Int,
    addTodoDueDate :: !(Maybe LocalTime)
  }

data UpdateTodoOpt = UpdateTodoOpt
  { updateTodoId :: !Int,
    updateTodoDescription :: !(Maybe Text),
    updateTodoPriority :: !(Maybe Int),
    updateTodoDueDate :: !(Maybe LocalTime),
    updateTodoDone :: !(Maybe Bool)
  }
  deriving (Show)

emptyUpdateTodoOpt :: Int -> UpdateTodoOpt
emptyUpdateTodoOpt i = UpdateTodoOpt i Nothing Nothing Nothing Nothing

data WorkSpace = WorkSpace
  { wsRoot :: !FilePath,
    wsDbName :: !String
  }

getDbPath :: WorkSpace -> FilePath
getDbPath ws = wsRoot ws <> "/" <> wsDbName ws

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options,
    -- Add other app-specific configuration information here
    appWorkSpace :: !WorkSpace
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
