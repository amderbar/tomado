{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Model.Todo where

import Data.Int (Int32)
import Database.Beam

newtype TodoT f = Todo { _todoId :: Columnar f Int32 } deriving (Generic, Beamable)

instance Table TodoT where
  data PrimaryKey TodoT f = TodoId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = TodoId . _todoId

type Todo = TodoT Identity

deriving instance Show Todo
deriving instance Eq Todo

type TodoId = PrimaryKey TodoT Identity

deriving instance Show TodoId
deriving instance Eq TodoId

type TodoParentId = PrimaryKey TodoT (Nullable Identity)

deriving instance Show TodoParentId
deriving instance Eq TodoParentId
