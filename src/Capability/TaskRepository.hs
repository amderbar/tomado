module Capability.TaskRepository (
    TaskRepository(..)
) where

import Data.TomadoTask (TomadoTask, TomadoTaskId)

class Monad repo => TaskRepository repo where
    createTask :: TomadoTask -> repo ()
    readTask :: TomadoTaskId -> repo (Maybe TomadoTask)
    updateTask :: TomadoTask -> repo ()
    deleteTask :: TomadoTaskId -> repo ()
    listTasks :: repo [TomadoTask]
