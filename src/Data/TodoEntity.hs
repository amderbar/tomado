{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.TodoEntity where

import Data.Comment (Comment)
import Data.Context (Context)
import Data.Project (Project)
import Data.Tag (Tag)
import Import
  ( Bool (..),
    Display (textDisplay),
    Eq,
    ISO8601 (iso8601Format),
    Identity (Identity, runIdentity),
    Int,
    Maybe (..),
    Show,
    catMaybes,
    formatShow,
    fromMaybe,
    ($),
    (.),
    (<$>),
    (<>),
  )
import RIO.Text (Text, empty, pack, unwords)
import RIO.Time (LocalTime)

newtype TodoId = TodoId Int
  deriving (Eq, Show, Display)

data TodoEntityT m = TodoEntity
  { _todoId :: m TodoId,
    todoDescription :: Text,
    todoDetail :: Text,
    todoDone :: Bool,
    _todoCreatedAt :: m LocalTime,
    todoUpdatedAt :: Maybe LocalTime,
    todoPriority :: Int,
    todoDueDate :: Maybe LocalTime,
    todoContext :: Maybe Context,
    todoProject :: Maybe Project,
    todoTags :: [Tag],
    todoParent :: Maybe TodoId,
    todoComments :: [Comment]
  }

deriving instance (Show (f TodoId), Show (f LocalTime)) => Show (TodoEntityT f)

deriving instance (Eq (f TodoId), Eq (f LocalTime)) => Eq (TodoEntityT f)

instance Display TodoEntity where
  textDisplay t =
    let todoProps =
          catMaybes
            [ Just $ if todoDone t then "[x]" else "[ ]",
              Just $ textDisplay (todoId t),
              Just (todoDescription t),
              (<>) "due:" . fmtDisplay <$> todoDueDate t,
              textDisplay <$> todoProject t,
              textDisplay <$> todoContext t
            ]
        tags = textDisplay <$> todoTags t
     in unwords (todoProps <> tags)
    where
      fmtDisplay :: LocalTime -> Text
      fmtDisplay = pack . formatShow iso8601Format

type TodoEntity = TodoEntityT Identity

todoId :: TodoEntity -> TodoId
todoId = runIdentity . _todoId

todoCreatedAt :: TodoEntity -> LocalTime
todoCreatedAt = runIdentity . _todoCreatedAt

type NewTodoEntity = TodoEntityT Maybe

emptyTodoEntity :: NewTodoEntity
emptyTodoEntity =
  TodoEntity
    { _todoId = Nothing,
      todoDescription = empty,
      todoDetail = empty,
      todoDone = False,
      _todoCreatedAt = Nothing,
      todoUpdatedAt = Nothing,
      todoPriority = 0,
      todoDueDate = Nothing,
      todoContext = Nothing,
      todoProject = Nothing,
      todoTags = [],
      todoParent = Nothing,
      todoComments = []
    }

concreteTodoEntity :: TodoId -> LocalTime -> NewTodoEntity -> TodoEntity
concreteTodoEntity tid createdAt todo =
  let _tid = Identity $ fromMaybe tid (_todoId todo)
      _createdAt = Identity $ fromMaybe createdAt (_todoCreatedAt todo)
   in todo
        { _todoId = _tid,
          _todoCreatedAt = _createdAt
        }
