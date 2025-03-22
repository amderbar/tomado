{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Model.Event.TodoTrashed
  ( TodoTrashedT (..),
    TodoTrashed,
    TodoTrashedId,
    PrimaryKey (..),
  )
where

import Data.Int (Int32)
import Database.Beam
import Database.Model.Event
import Database.Model.Todo

data TodoTrashedT f = TodoTrashed
  { _todoTrashedId :: Columnar f Int32,
    _todoTrashedEvent :: PrimaryKey EventT f,
    _todoTrashedTodo :: PrimaryKey TodoT f,
    _todoIsTrashed :: Columnar f Bool
  }
  deriving (Generic, Beamable)

instance Table TodoTrashedT where
  data PrimaryKey TodoTrashedT f = TodoTrashedId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = TodoTrashedId . _todoTrashedId

type TodoTrashed = TodoTrashedT Identity

deriving instance Show TodoTrashed

deriving instance Eq TodoTrashed

type TodoTrashedId = PrimaryKey TodoTrashedT Identity

deriving instance Show TodoTrashedId

deriving instance Eq TodoTrashedId
