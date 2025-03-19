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
  addedTodo <- liftIO $ withConnection (getDbPath ws) $ runAppM (addTodo newTodo)
  printBuilderLn (display addedTodo)
  printBuilderLn "--"
  printBuilderLn $ "TODO: " <> display (todoId addedTodo) <> " added"
  where
    addTodo newTodo = do
      (i, createdAt) <- createTodoEntry newTodo nullEventId
      let td = concreteTodoEntity i createdAt newTodo
      -- TODO: ここの判定はもっと汎用的に使えるものにしたいなあ
      if addTodoPriority /= 0 || isJust addTodoDueDate
        then do
          -- FIXME: ここではEventIdが取れないので正しいparentEventIdを渡せない
          updatedAt <- updateTodoEntry td nullEventId
          pure td {todoUpdatedAt = Just updatedAt}
        else
          pure td

listTodoAction :: AppM App ()
listTodoAction = do
  ws <- asks appWorkSpace
  todos <- liftIO $ withConnection (getDbPath ws) $ runAppM listTodoEntries
  forM_ todos (printBuilderLn . display)
  printBuilderLn "--"
  printBuilderLn $ "Total: " <> display (length todos) <> " todos"

printBuilderLn :: (MonadIO m) => Utf8Builder -> m ()
printBuilderLn = liftIO . hPutBuilder stdout . getUtf8Builder . (<> "\n")
