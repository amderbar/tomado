{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Schema
  ( TomadoDb (..),
    tomadoDb,
  )
where

import Database.Beam
import Database.Model.Event (EventT)
import Database.Model.Event.TodoCreated
import Database.Model.Event.TodoDoneToggled
import Database.Model.Event.TodoUpdated
import Database.Model.Todo (TodoT)

data TomadoDb f = TomadoDb
  { _tomadoDbEvents :: f (TableEntity EventT),
    _tomadoDbTodo :: f (TableEntity TodoT),
    _tomadoDbTodoCreated :: f (TableEntity TodoCreatedT),
    _tomadoDbTodoUpdated :: f (TableEntity TodoUpdatedT),
    _tomadoDbTodoDoneToggled :: f (TableEntity TodoDoneToggledT)
  }
  deriving (Generic, Database be)

tomadoDb :: DatabaseSettings be TomadoDb
tomadoDb = defaultDbSettings
