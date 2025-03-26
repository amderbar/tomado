{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Model.Event
  ( EventT (..),
    Event,
    EventId,
    EventParentId,
  )
where

import Data.Int (Int32)
import Database.Beam
import RIO.Time (LocalTime)

data EventT f = Event
  { _eventId :: Columnar f Int32,
    _eventParent :: PrimaryKey EventT (Nullable f),
    _eventOccurredAt :: Columnar f LocalTime
  }
  deriving (Generic)

instance Beamable EventT

instance Table EventT where
  data PrimaryKey EventT f = EventId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = EventId . _eventId

type Event = EventT Identity

deriving instance Show Event

deriving instance Eq Event

type EventId = PrimaryKey EventT Identity

deriving instance Show EventId

deriving instance Eq EventId

type EventParentId = PrimaryKey EventT (Nullable Identity)

deriving instance Show EventParentId

deriving instance Eq EventParentId
