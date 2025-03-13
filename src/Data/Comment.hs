{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Comment where

import Data.Int (Int32)
import Import (Identity (runIdentity))
import RIO.Text (Text, empty)
import RIO.Time (LocalTime)

newtype CommentId = CommentId Int32
  deriving (Eq, Show)

data CommentT m = Comment
  { _commentId :: m CommentId,
    commentText :: Text,
    _commentCreatedAt :: m LocalTime,
    commentUpdatedAt :: Maybe LocalTime
  }

deriving instance (Show (f CommentId), Show (f LocalTime)) => Show (CommentT f)

deriving instance (Eq (f CommentId), Eq (f LocalTime)) => Eq (CommentT f)

type Comment = CommentT Identity

commentId :: Comment -> CommentId
commentId = runIdentity . _commentId

commentCreatedAt :: Comment -> LocalTime
commentCreatedAt = runIdentity . _commentCreatedAt

type NewComment = CommentT Maybe

emptyComment :: NewComment
emptyComment =
  Comment
    { _commentId = Nothing,
      commentText = empty,
      _commentCreatedAt = Nothing,
      commentUpdatedAt = Nothing
    }
