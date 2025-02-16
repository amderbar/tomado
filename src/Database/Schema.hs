{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Schema where

import Data.Int (Int32)
import Database.Beam
import RIO.Text (Text)
import RIO.Time (LocalTime)
import Data.TomadoTask

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

data TomadoDb f = TomadoDb
  { _tomadoDbTasks :: f (TableEntity TaskT)
  }
  deriving (Generic, Database be)

tomadoDb :: DatabaseSettings be TomadoDb
tomadoDb = defaultDbSettings
