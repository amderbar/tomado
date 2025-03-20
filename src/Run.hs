{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Capability.TodoReadWritable
import Control.Monad.Trans.Cont
import Data.TodoEntity
  ( TodoEntityT (..),
    concreteTodoEntity,
    emptyTodoEntity,
    todoId,
  )
import Database.Model.Event (nullEventId)
import Database.Setup (initDb)
import Database.Util (withConnection)
import Import
import RIO.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)
import RIO.FilePath (addExtension)
import RIO.Process (mkDefaultProcessContext)
import System.Environment (getProgName)
import Tomado

run :: Options -> IO ()
run args = evalContT $ do
  progName <- liftIO getProgName
  wd <- liftIO $ getXdgDirectory XdgData progName
  lo <- liftIO $ logOptionsHandle stderr (optionsVerbose args)
  pc <- liftIO mkDefaultProcessContext
  lf <- ContT (withLogFunc lo)
  let app =
        App
          { appLogFunc = lf,
            appProcessContext = pc,
            appOptions = args,
            appWorkSpace = WorkSpace wd (addExtension progName "db")
          }
  liftIO $ runAppM router app

router :: AppM App ()
router = do
  Options {optionsAction} <- asks appOptions
  case optionsAction of
    Init _ -> initAction
    Config _ -> logInfo "Config"
    ListTodo _ -> listTodoAction
    AddTodo opt -> addTodoAction opt
    UpdateTodo _ -> logInfo "UpdateTodo"

initAction :: AppM App ()
initAction = do
  ws <- asks appWorkSpace
  liftIO $ createDirectoryIfMissing False (wsRoot ws)
  liftIO $ withConnection (getDbPath ws) (liftIO . initDb)
  logInfo "Initialized database"

addTodoAction :: AddTodoOpt -> AppM App ()
addTodoAction AddTodoOpt {addTodoDescription, addTodoPriority, addTodoDueDate} = do
  let newTodo =
        emptyTodoEntity
          { todoDescription = addTodoDescription,
            todoPriority = addTodoPriority,
            todoDueDate = addTodoDueDate
          }
  ws <- asks appWorkSpace
  addedTodo <- liftIO $ withConnection (getDbPath ws) $ runAppM $ do
    (i, createdAt, updatedAt) <- createTodoEntry newTodo nullEventId
    pure (concreteTodoEntity i createdAt newTodo) {todoUpdatedAt = updatedAt}
  printBuilderLn (display addedTodo)
  printBuilderLn "--"
  logInfo $ "TODO: " <> display (todoId addedTodo) <> " added"

listTodoAction :: AppM App ()
listTodoAction = do
  ws <- asks appWorkSpace
  todos <- liftIO $ withConnection (getDbPath ws) $ runAppM listTodoEntries
  forM_ todos (printBuilderLn . display)
  printBuilderLn "--"
  logInfo $ "Total: " <> display (length todos) <> " todos"

printBuilderLn :: (MonadIO m) => Utf8Builder -> m ()
printBuilderLn = liftIO . hPutBuilder stdout . getUtf8Builder . (<> "\n")
