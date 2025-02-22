{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Todo where

import Data.Comment (Comment)
import Data.Int (Int32)
import Data.Project (Project)
import Import (Identity (runIdentity))
import RIO.Text (Text, empty)
import RIO.Time (LocalTime)

newtype TodoId = TodoId Int32
  deriving (Eq, Show)

data TodoT m = Todo
  { _todoId :: m TodoId,
    todoName :: Text,
    todoDescription :: Text,
    todoDone :: Bool,
    _todoCreatedAt :: m LocalTime,
    _todoUpdatedAt :: m LocalTime,
    todoPriority :: Int32,
    todoDueDate :: Maybe LocalTime,
    todoContext :: Maybe Text,
    todoProject :: Maybe Project,
    todoTags :: [Text],
    todoParent :: Maybe TodoId,
    todoComments :: [Comment]
  }

deriving instance (Show (f TodoId), Show (f LocalTime)) => Show (TodoT f)

deriving instance (Eq (f TodoId), Eq (f LocalTime)) => Eq (TodoT f)

type Todo = TodoT Identity

todoId :: Todo -> TodoId
todoId = runIdentity . _todoId

todoCreatedAt :: Todo -> LocalTime
todoCreatedAt = runIdentity . _todoCreatedAt

todoUpdatedAt :: Todo -> LocalTime
todoUpdatedAt = runIdentity . _todoUpdatedAt

type NewTodo = TodoT Maybe

emptyTodo :: NewTodo
emptyTodo =
  Todo
    { _todoId = Nothing,
      todoName = empty,
      todoDescription = empty,
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
