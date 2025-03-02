{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Model.Event where

import RIO.Time (LocalTime)
import Data.Int (Int32)
import Database.Beam

data EventT f = Event
  { _eventId :: Columnar f Int32,
    _eventParent  :: PrimaryKey EventT (Nullable f),
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

nullEventId :: EventParentId
nullEventId = EventId Nothing
