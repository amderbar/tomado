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
import RIO.Directory (XdgDirectory (XdgData), createDirectory, getXdgDirectory)
import RIO.FilePath (takeBaseName, (<.>))
import RIO.Process (mkDefaultProcessContext)
import qualified RIO.Text as T
import System.Environment (getProgName)
import System.IO (getContents)
import System.IO.Error (catchIOError, ioError, isAlreadyExistsError)
import Tomado (AppM, runAppM)

run :: Options -> IO ()
run args = evalContT $ do
  lo <- liftIO $ logOptionsHandle stderr (optionsVerbose args)
  pc <- liftIO mkDefaultProcessContext
  wd <- liftIO $ getXdgDirectory XdgData =<< getProgName
  ws <- liftIO (prepareWorkspace wd)
  cn <- ContT $ withConnection (getDbPath ws)
  lf <- ContT (withLogFunc lo)
  let app =
        App
          { appLogFunc = lf,
            appProcessContext = pc,
            appOptions = args,
            appConnection = cn,
            appWorkSpace = ws
          }
  liftIO $ runAppM router app
  where
    prepareWorkspace :: FilePath -> IO WorkSpace
    prepareWorkspace rootPath = do
      let db = takeBaseName rootPath <.> "db"
        ws = WorkSpace rootPath db
      catchIOError (initWorkspace ws) $ \e ->
        if isAlreadyExistsError e
          then pure ()
          else ioError e
      pure ws

    initWorkspace :: WorkSpace -> IO ()
    initWorkspace ws = do
      createDirectory (wsRoot ws)
      withConnection (getDbPath ws) initDb

router :: AppM App ()
router = do
  Options {optionsAction} <- asks appOptions
  case optionsAction of
    Config _ -> logInfo "Config"
    ListTodo _ -> listTodoAction
    AddTodo opt -> logDebug (displayShow opt) >> addTodoAction opt
    UpdateTodo opt -> logDebug (displayShow opt) >> updateTodoAction opt
    TrashTodo opt -> logDebug (displayShow opt) >> trashTodoAction opt

addTodoAction :: AddTodoOpt -> AppM App ()
addTodoAction AddTodoOpt {addTodoDescription, addTodoPriority, addTodoDueDate, addTodoDetail} = do
  addTodoDetailContents <-
    if addTodoDetail
      then do
        printBuilderLn "Enter the detail and press Ctrl-D when finished."
        printBuilderLn "--"
        Just . T.pack <$> liftIO getContents
      else pure Nothing
  let newTodo =
        emptyTodoEntity
          { todoDescription = addTodoDescription,
            todoPriority = addTodoPriority,
            todoDueDate = addTodoDueDate
          }
          & (\t -> maybe t (\u -> t {todoDetail = u}) addTodoDetailContents)
  (i, createdAt, updatedAt) <- createTodoEntry newTodo nothing_
  let addedTodo = concreteTodoEntity i createdAt newTodo {todoUpdatedAt = updatedAt}
  printBuilderLn (display addedTodo)
  printBuilderLn "--"
  logInfo $ "TODO: " <> display (todoId addedTodo) <> " added"

updateTodoAction :: UpdateTodoOpt -> AppM App ()
updateTodoAction UpdateTodoOpt {updateTodoId, updateTodoDescription, updateTodoPriority, updateTodoDueDate, updateTodoDone} = do
  target <- readTodoEntry (TodoId updateTodoId)
  ret <- forM target updateTargetTodo
  case ret of
    Nothing -> logError $ "No such todo: " <> display updateTodoId
    Just updatedTodo -> do
      printBuilderLn (display updatedTodo)
      printBuilderLn "--"
      logInfo $ "TODO: " <> display (todoId updatedTodo) <> " updated"
  where
    updateTargetTodo :: TodoEntity -> AppM App TodoEntity
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

trashTodoAction :: TrashTodoOpt -> AppM App ()
trashTodoAction TrashTodoOpt {trashTodoId} = do
  ws <- asks appWorkSpace
  ret <- liftIO $ withConnection (getDbPath ws) $ runAppM $ do
    target <- readTodoEntry (TodoId trashTodoId)
    forM target trashTargetTodo
  case ret of
    Nothing -> logError $ "No such todo: " <> display trashTodoId
    Just _ -> logInfo $ "TODO: " <> display trashTodoId <> " trashed"
  where
    trashTargetTodo :: TodoEntity -> AppM Connection ()
    trashTargetTodo target = void $ deleteTodoEntry (todoId target) nothing_

listTodoAction :: AppM App ()
listTodoAction = do
  ws <- asks appWorkSpace
  todos <- liftIO $ withConnection (getDbPath ws) $ runAppM listTodoEntries
  forM_ todos (printBuilderLn . display)
  printBuilderLn "--"
  logInfo $ "Total: " <> display (length todos) <> " todos"

printBuilderLn :: (MonadIO m) => Utf8Builder -> m ()
printBuilderLn = liftIO . hPutBuilder stdout . getUtf8Builder . (<> "\n")
