{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Schema where

import Data.Int (Int32)
import Data.TomadoTask
import Database.Beam
import RIO.Text (Text)
import RIO.Time (LocalTime)

data TomadoDb f = TomadoDb
  { _tomadoDbTasks :: f (TableEntity TaskT),
    _tomadoDbProjectRevisions :: f (TableEntity ProjectRevisionT)
  }
  deriving (Generic, Database be)

tomadoDb :: DatabaseSettings be TomadoDb
tomadoDb = defaultDbSettings

data ProjectRevisionT f = ProjectRevision
  { _projectRevisionId :: Columnar f Int32,
    _projectRevisionCreatedAt :: Columnar f LocalTime,
    _projectId :: Columnar f Int32,
    _projectName :: Columnar f Text,
    _projectDescription :: Columnar f Text,
    _projectDueDate :: Columnar f (Maybe LocalTime),
    _projectParent :: Columnar f (Maybe Int32)
  }
  deriving (Generic, Beamable)

type ProjectRevision = ProjectRevisionT Identity

type ProjectRevisionId = PrimaryKey ProjectRevisionT Identity

instance Table ProjectRevisionT where
  data PrimaryKey ProjectRevisionT f = ProjectRevisionId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = ProjectRevisionId . _projectRevisionId

data CommentT f = Comment
  { _commentId :: Columnar f Int32,
    _commentContent :: Columnar f Text,
    _commentCreatedAt :: Columnar f LocalTime
  }
  deriving (Generic, Beamable)

data TodoT f = Todo
  { _todoId :: Columnar f Int32,
    _todoName :: Columnar f Text,
    _todoDescription :: Columnar f Text,
    _todoCreatedAt :: Columnar f LocalTime
  }
  deriving (Generic, Beamable)

data TaskT f = Task
  { _taskId :: Columnar f Int32,
    _taskNum :: Columnar f Text,
    _taskName :: Columnar f Text,
    _taskDescription :: Columnar f Text,
    _taskCreatedAt :: Columnar f LocalTime
  }
  deriving (Generic, Beamable)

type Task = TaskT Identity

type TaskId = PrimaryKey TaskT Identity

deriving instance Show Task

instance Table TaskT where
  data PrimaryKey TaskT f = TaskId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = TaskId . _taskId

toTomadoTask :: Task -> TomadoTask
toTomadoTask (Task _ i n d _) = TomadoTask (TomadoTaskId i) n d
