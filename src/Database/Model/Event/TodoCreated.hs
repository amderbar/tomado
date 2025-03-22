{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Model.Event.TodoCreated
  ( TodoCreatedT (..),
    TodoCreated,
    TodoCreatedId,
    PrimaryKey (..),
  )
where

import Data.Int (Int32)
import Database.Beam
import Database.Model.Event
import Database.Model.Todo
import RIO.Text (Text)

data TodoCreatedT f = TodoCreated
  { _todoCreatedId :: Columnar f Int32,
    _todoCreatedEvent :: PrimaryKey EventT f,
    _todoCreatedTodo :: PrimaryKey TodoT f,
    _todoCreatedDescription :: Columnar f Text
  }
  deriving (Generic, Beamable)

instance Table TodoCreatedT where
  data PrimaryKey TodoCreatedT f = TodoCreatedId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = TodoCreatedId . _todoCreatedId

type TodoCreated = TodoCreatedT Identity

deriving instance Show TodoCreated

deriving instance Eq TodoCreated

type TodoCreatedId = PrimaryKey TodoCreatedT Identity

deriving instance Show TodoCreatedId

deriving instance Eq TodoCreatedId
