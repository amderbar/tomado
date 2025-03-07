{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tag where

import Data.Int (Int32)
import Import (Identity (runIdentity))
import RIO.Text (Text, empty)
import RIO.Time (LocalTime)

newtype TagId = TagId Int32
  deriving (Eq, Show)

data TagT m = Tag
  { _tagId :: m TagId,
    tagName :: Text,
    _tagCreatedAt :: m LocalTime,
    tagUpdatedAt :: Maybe LocalTime
  }

deriving instance (Show (f TagId), Show (f LocalTime)) => Show (TagT f)

deriving instance (Eq (f TagId), Eq (f LocalTime)) => Eq (TagT f)

type Tag = TagT Identity

tagId :: Tag -> TagId
tagId = runIdentity . _tagId

tagCreatedAt :: Tag -> LocalTime
tagCreatedAt = runIdentity . _tagCreatedAt

type NewTag = TagT Maybe

emptyTag :: NewTag
emptyTag =
  Tag
    { _tagId = Nothing,
      tagName = empty,
      _tagCreatedAt = Nothing,
      tagUpdatedAt = Nothing
    }
