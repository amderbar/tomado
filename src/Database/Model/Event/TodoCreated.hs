{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Model.Event.TodoCreated where

import RIO.Text (Text)
import Data.Int (Int32)
import Database.Beam
import Database.Model.Event
import Database.Model.Todo

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
