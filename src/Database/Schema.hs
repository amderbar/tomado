{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Schema
  ( TomadoDb (..),
    tomadoDb,
    module Database.TaskTable,
  )
where

import Database.Beam
import Database.Model.Event (EventT)
import Database.Model.Event.TodoCreated
import Database.Model.Event.TodoDoneToggled
import Database.Model.Event.TodoUpdated
import Database.Model.Todo (TodoT)
import Database.TaskTable

data TomadoDb f = TomadoDb
  { _tomadoDbTasks :: f (TableEntity TaskT),
    _tomadoDbEvents :: f (TableEntity EventT),
    _tomadoDbTodo :: f (TableEntity TodoT),
    _tomadoDbTodoCreated :: f (TableEntity TodoCreatedT),
    _tomadoDbTodoUpdated :: f (TableEntity TodoUpdatedT),
    _tomadoDbTodoDoneToggled :: f (TableEntity TodoDoneToggledT)
  }
  deriving (Generic, Database be)

tomadoDb :: DatabaseSettings be TomadoDb
tomadoDb = defaultDbSettings
