{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Query where

import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend (SqlNull)
import Database.Model.Event (Event, EventT (_eventId))
import Database.Model.Event.TodoCreated (TodoCreated, TodoCreatedT (_todoCreatedEvent, _todoCreatedTodo))
import Database.Model.Event.TodoUpdated (TodoUpdated, TodoUpdatedT (_todoUpdatedEvent, _todoUpdatedTodo))
import Database.Schema (TomadoDb (_tomadoDbEvents, _tomadoDbTodoCreated, _tomadoDbTodoUpdated), tomadoDb)
import Import (Text)
import RIO.Time (LocalTime)

allTodoEntries ::
  ( HasQBuilder be,
    FromBackendRow be Int32,
    FromBackendRow be Text,
    FromBackendRow be LocalTime,
    FromBackendRow be SqlNull,
    FromBackendRow be Bool,
    HasSqlEqualityCheck be Int32,
    HasSqlEqualityCheck be LocalTime,
    MonadBeam be m
  ) =>
  m [(TodoCreated, Event, Maybe TodoUpdated, Maybe Event)]
allTodoEntries = runSelectReturningList $ select $ do
  ev <- all_ (_tomadoDbEvents tomadoDb)
  crd <- oneToOne_ (_tomadoDbTodoCreated tomadoDb) _todoCreatedEvent ev
  (upd, updEv) <- leftJoin_ getAllLatestTodoUpdate (\(u, _) -> _todoUpdatedTodo u ==. _todoCreatedTodo crd)
  pure (crd, ev, upd, updEv)

getAllLatestTodoUpdate ::
  ( Database be TomadoDb,
    HasTableEquality be EventT,
    HasSqlEqualityCheck be Int32
  ) =>
  Q be TomadoDb s (TodoUpdatedT (QExpr be s), EventT (QExpr be s))
getAllLatestTodoUpdate = do
  (_, eid) <- aggregate_ (\(u, e) -> (group_ $ _todoUpdatedTodo u, max_ (_eventId e))) $ do
    e <- all_ (_tomadoDbEvents tomadoDb)
    u <- oneToMany_ (_tomadoDbTodoUpdated tomadoDb) _todoUpdatedEvent e
    pure (u, e)
  e <- join_' (_tomadoDbEvents tomadoDb) (\e -> just_ (_eventId e) ==?. eid)
  upd <- oneToMany_ (_tomadoDbTodoUpdated tomadoDb) _todoUpdatedEvent e
  pure (upd, e)
