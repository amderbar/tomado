{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Project where

import Data.Int (Int32)
import Import (Identity (runIdentity))
import RIO.Text (Text, empty)
import RIO.Time (LocalTime)

newtype ProjectId = ProjectId Int32
  deriving (Eq, Show)

data ProjectT m = Project
  { _projectId :: m ProjectId,
    projectName :: Text,
    projectDescription :: Text,
    projectDueDate :: Maybe LocalTime,
    projectParent :: Maybe Project,
    _projectCreatedAt :: m LocalTime,
    _projectUpdatedAt :: m LocalTime
  }

deriving instance (Show (f ProjectId), Show (f LocalTime)) => Show (ProjectT f)

deriving instance (Eq (f ProjectId), Eq (f LocalTime)) => Eq (ProjectT f)

fullName :: ProjectT f -> Text
fullName p = parentFullName p <> "/" <> projectName p
  where
    parentFullName :: ProjectT f -> Text
    parentFullName p' = case projectParent p' of
      Just pa -> fullName pa
      Nothing -> projectName p'

type Project = ProjectT Identity

projectId :: Project -> ProjectId
projectId = runIdentity . _projectId

projectCreatedAt :: Project -> LocalTime
projectCreatedAt = runIdentity . _projectCreatedAt

projectUpdatedAt :: Project -> LocalTime
projectUpdatedAt = runIdentity . _projectUpdatedAt

type NewProject = ProjectT Maybe

emptyProject :: NewProject
emptyProject =
  Project
    { _projectId = Nothing,
      projectName = empty,
      projectDescription = empty,
      projectDueDate = Nothing,
      projectParent = Nothing,
      _projectCreatedAt = Nothing,
      _projectUpdatedAt = Nothing
    }
