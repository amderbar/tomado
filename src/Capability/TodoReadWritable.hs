module Capability.TodoReadWritable
  ( TodoReadWritable,
    TodoReadable (..),
    TodoWritable (..),
  )
where

import Data.TodoEntity (NewTodoEntity, TodoEntity, TodoId)
import Database.Model.Event (EventParentId) -- TODO: DB関連モジュールに直接依存するのはやめたい
import RIO.Time (LocalTime)

class (TodoReadable repo, TodoWritable repo) => TodoReadWritable repo

class (Monad repo) => TodoReadable repo where
  readTodoEntry :: TodoId -> repo (Maybe TodoEntity)
  listTodoEntries :: repo [TodoEntity]

class (Monad repo) => TodoWritable repo where
  createTodoEntry :: NewTodoEntity -> EventParentId -> repo (TodoId, LocalTime)
  updateTodoEntry :: TodoEntity -> EventParentId -> repo LocalTime
  deleteTodoEntry :: TodoId -> EventParentId -> repo LocalTime
