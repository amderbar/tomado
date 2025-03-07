{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Context where

import Data.Int (Int32)
import Import (Identity (runIdentity))
import RIO.Text (Text, empty)
import RIO.Time (LocalTime)

newtype ContextId = ContextId Int32
  deriving (Eq, Show)

data ContextT m = Context
  { _contextId :: m ContextId,
    contextName :: Text,
    contextDescription :: Text,
    _contextCreatedAt :: m LocalTime,
    contextUpdatedAt :: Maybe LocalTime
  }

deriving instance (Show (f ContextId), Show (f LocalTime)) => Show (ContextT f)

deriving instance (Eq (f ContextId), Eq (f LocalTime)) => Eq (ContextT f)

type Context = ContextT Identity

contextId :: Context -> ContextId
contextId = runIdentity . _contextId

contextCreatedAt :: Context -> LocalTime
contextCreatedAt = runIdentity . _contextCreatedAt

type NewContext = ContextT Maybe

emptyContext :: NewContext
emptyContext =
  Context
    { _contextId = Nothing,
      contextName = empty,
      contextDescription = empty,
      _contextCreatedAt = Nothing,
      contextUpdatedAt = Nothing
    }
