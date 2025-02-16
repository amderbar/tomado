module Data.TomadoTask (
    TomadoTaskId(..),
    TomadoTask(..)
) where

import RIO.Text (Text)

newtype TomadoTaskId = TomadoTaskId Text
    deriving (Eq, Show)

data TomadoTask = TomadoTask {
    taskId :: TomadoTaskId,
    taskName :: Text,
    taskDescription :: Text
} deriving (Eq, Show)
