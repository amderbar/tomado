{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Model.Event.TodoUpdated where

import RIO.Text (Text)
import RIO.Time (LocalTime)
import Data.Int (Int32)
import Database.Beam
import Database.Model.Event
import Database.Model.Todo

data TodoUpdatedT f = TodoUpdated
  { _todoUpdatedId :: Columnar f Int32,
    _todoUpdatedEvent :: PrimaryKey EventT f,
    _todoUpdatedTodo :: PrimaryKey TodoT f,
    _todoUpdatedDescription :: Columnar f Text,
    _todoUpdatedDetail :: Columnar f Text,
    _todoUpdatedDone :: Columnar f Bool,
    _todoUpdatedPriority :: Columnar f Int32,
    _todoUpdatedDueDate :: Columnar f (Maybe LocalTime),
    -- _todoUpdatedContext :: Maybe Context,
    -- _todoUpdatedProject :: Maybe Project,
    _todoUpdatedParent :: PrimaryKey TodoT (Nullable f)
  }
  deriving (Generic, Beamable)

instance Table TodoUpdatedT where
    data PrimaryKey TodoUpdatedT f = TodoUpdatedId (Columnar f Int32) deriving (Generic, Beamable)
    primaryKey = TodoUpdatedId . _todoUpdatedId

type TodoUpdated = TodoUpdatedT Identity

deriving instance Show TodoUpdated
deriving instance Eq TodoUpdated

type TodoUpdatedId = PrimaryKey TodoUpdatedT Identity

deriving instance Show TodoUpdatedId
deriving instance Eq TodoUpdatedId
