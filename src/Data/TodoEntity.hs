{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TodoEntity where

import Data.Comment (Comment)
import Data.Context (Context)
import Data.Int (Int32)
import Data.Project (Project)
import Data.Tag (Tag)
import Import (Identity (runIdentity))
import RIO.Text (Text, empty)
import RIO.Time (LocalTime)

newtype TodoId = TodoId Int32
  deriving (Eq, Show)

data TodoEntityT m = TodoEntity
  { _todoId :: m TodoId,
    todoDescription :: Text,
    todoDetail :: Text,
    todoDone :: Bool,
    _todoCreatedAt :: m LocalTime,
    _todoUpdatedAt :: m LocalTime,
    todoPriority :: Int32,
    todoDueDate :: Maybe LocalTime,
    todoContext :: Maybe Context,
    todoProject :: Maybe Project,
    todoTags :: [Tag],
    todoParent :: Maybe TodoId,
    todoComments :: [Comment]
  }

deriving instance (Show (f TodoId), Show (f LocalTime)) => Show (TodoEntityT f)

deriving instance (Eq (f TodoId), Eq (f LocalTime)) => Eq (TodoEntityT f)

type TodoEntity = TodoEntityT Identity

todoId :: TodoEntity -> TodoId
todoId = runIdentity . _todoId

todoCreatedAt :: TodoEntity -> LocalTime
todoCreatedAt = runIdentity . _todoCreatedAt

todoUpdatedAt :: TodoEntity -> LocalTime
todoUpdatedAt = runIdentity . _todoUpdatedAt

type NewTodoEntity = TodoEntityT Maybe

emptyTodoEntity :: NewTodoEntity
emptyTodoEntity =
  TodoEntity
    { _todoId = Nothing,
      todoDescription = empty,
      todoDetail = empty,
      todoDone = False,
      _todoCreatedAt = Nothing,
      _todoUpdatedAt = Nothing,
      todoPriority = 0,
      todoDueDate = Nothing,
      todoContext = Nothing,
      todoProject = Nothing,
      todoTags = [],
      todoParent = Nothing,
      todoComments = []
    }
