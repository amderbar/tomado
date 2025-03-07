{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Model.Event.TodoDoneToggled where

import Data.Int (Int32)
import Database.Beam
import Database.Model.Event
import Database.Model.Todo

data TodoDoneToggledT f = TodoDoneToggled
  { _todoDoneToggledId :: Columnar f Int32,
    _todoDoneToggledEvent :: PrimaryKey EventT f,
    _todoDoneToggledTodo :: PrimaryKey TodoT f,
    _todoDoneToggledDone :: Columnar f Bool
  }
  deriving (Generic, Beamable)

instance Table TodoDoneToggledT where
    data PrimaryKey TodoDoneToggledT f = TodoDoneToggledId (Columnar f Int32) deriving (Generic, Beamable)
    primaryKey = TodoDoneToggledId . _todoDoneToggledId

type TodoDoneToggled = TodoDoneToggledT Identity

deriving instance Show TodoDoneToggled
deriving instance Eq TodoDoneToggled

type TodoDoneToggledId = PrimaryKey TodoDoneToggledT Identity

deriving instance Show TodoDoneToggledId
deriving instance Eq TodoDoneToggledId
