{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Capability.TodoReadWritable
import Control.Monad.Trans.Cont
import Data.TodoEntity
  ( TodoEntity,
    TodoEntityT (..),
    TodoId (TodoId),
    concreteTodoEntity,
    emptyTodoEntity,
    todoId,
  )
import Database.Setup (initDb)
import Database.Util (Connection, SqlJustable (nothing_), withConnection)
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
    AddTodo opt -> logDebug (displayShow opt) >> addTodoAction opt
    UpdateTodo opt -> logDebug (displayShow opt) >> updateTodoAction opt

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
    (i, createdAt, updatedAt) <- createTodoEntry newTodo nothing_
    pure (concreteTodoEntity i createdAt newTodo) {todoUpdatedAt = updatedAt}
  printBuilderLn (display addedTodo)
  printBuilderLn "--"
  logInfo $ "TODO: " <> display (todoId addedTodo) <> " added"

updateTodoAction :: UpdateTodoOpt -> AppM App ()
updateTodoAction UpdateTodoOpt {updateTodoId, updateTodoDescription, updateTodoPriority, updateTodoDueDate, updateTodoDone} = do
  ws <- asks appWorkSpace
  ret <- liftIO $ withConnection (getDbPath ws) $ runAppM $ do
    target <- readTodoEntry (TodoId updateTodoId)
    forM target updateTargetTodo
  case ret of
    Nothing -> logError $ "No such todo: " <> display updateTodoId
    Just updatedTodo -> do
      printBuilderLn (display updatedTodo)
      printBuilderLn "--"
      logInfo $ "TODO: " <> display (todoId updatedTodo) <> " updated"
  where
    updateTargetTodo :: TodoEntity -> AppM Connection TodoEntity
    updateTargetTodo target = do
      let updatedTodo =
            target
              & (\t -> maybe t (\u -> t {todoDescription = u}) updateTodoDescription)
              & (\t -> maybe t (\u -> t {todoPriority = Just u}) updateTodoPriority)
              -- TODO: How to Due data unset?
              & (\t -> maybe t (\u -> t {todoDueDate = Just u}) updateTodoDueDate)
              & (\t -> maybe t (\u -> t {todoDone = u}) updateTodoDone)
      if target /= updatedTodo
        then do
          todoUpdatedAt <- Just <$> updateTodoEntry updatedTodo nothing_
          pure updatedTodo {todoUpdatedAt}
        else pure target

listTodoAction :: AppM App ()
listTodoAction = do
  ws <- asks appWorkSpace
  todos <- liftIO $ withConnection (getDbPath ws) $ runAppM listTodoEntries
  forM_ todos (printBuilderLn . display)
  printBuilderLn "--"
  logInfo $ "Total: " <> display (length todos) <> " todos"

printBuilderLn :: (MonadIO m) => Utf8Builder -> m ()
printBuilderLn = liftIO . hPutBuilder stdout . getUtf8Builder . (<> "\n")
