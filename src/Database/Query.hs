{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Query where

import Data.Int (Int32)
import Data.Kind (Type)
import Database.Beam
import Database.Beam.Backend (SqlNull)
import Database.Model.Event (Event, EventT (_eventId))
import Database.Model.Event.TodoCreated (TodoCreated, TodoCreatedT (_todoCreatedEvent, _todoCreatedTodo))
import Database.Model.Event.TodoTrashed (TodoTrashedT (_todoIsTrashed, _todoTrashedEvent, _todoTrashedTodo))
import Database.Model.Event.TodoUpdated (TodoUpdated, TodoUpdatedT (_todoUpdatedEvent, _todoUpdatedTodo))
import Database.Model.Todo (PrimaryKey (TodoId), TodoT)
import Database.Schema (TomadoDb (_tomadoDbEvents, _tomadoDbTodoCreated, _tomadoDbTodoTrashed, _tomadoDbTodoUpdated), tomadoDb)
import Import (Text)
import RIO.Time (LocalTime)

type TodoQueryReturn = (TodoCreated, Event, Maybe TodoUpdated, Maybe Event)

getLatestTodo ::
  ( HasQBuilder be,
    HasSqlEqualityCheck be Int32,
    HasSqlEqualityCheck be LocalTime,
    FromBackendRow be Int32,
    FromBackendRow be Text,
    FromBackendRow be LocalTime,
    FromBackendRow be SqlNull,
    FromBackendRow be Bool,
    MonadBeam be m
  ) =>
  Int32 ->
  m (Maybe TodoQueryReturn)
getLatestTodo tid = runSelectReturningOne $ select $ do
  ev <- all_ (_tomadoDbEvents tomadoDb)
  crd <- oneToOne_ (_tomadoDbTodoCreated tomadoDb) _todoCreatedEvent ev
  (upd, updEv) <- leftJoin_ getAllLatestTodoUpdate (\(u, _) -> _todoUpdatedTodo u ==. _todoCreatedTodo crd)
  guard_ $ _todoCreatedTodo crd ==. val_ (TodoId tid)
  pure (crd, ev, upd, updEv)

getAllLatestTodo ::
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
  m [TodoQueryReturn]
getAllLatestTodo = runSelectReturningList $ select $ do
  ev <- all_ (_tomadoDbEvents tomadoDb)
  crd <- oneToOne_ (_tomadoDbTodoCreated tomadoDb) _todoCreatedEvent ev
  (upd, updEv) <- leftJoin_ getAllLatestTodoUpdate (\(u, _) -> _todoUpdatedTodo u ==. _todoCreatedTodo crd)
  (trsh, _) <- leftJoin_ getAllLatestTodoTrashed (\(u, _) -> _todoTrashedTodo u ==. _todoCreatedTodo crd)
  guard_ $ not_ $ fromMaybe_ (val_ False) (_todoIsTrashed trsh)
  pure (crd, ev, upd, updEv)

getAllLatestTodoUpdate ::
  ( Database be TomadoDb,
    HasTableEquality be EventT,
    HasSqlEqualityCheck be Int32
  ) =>
  Q be TomadoDb s (TodoUpdatedT (QExpr be s), EventT (QExpr be s))
getAllLatestTodoUpdate = getAllLatestTodoEvents _tomadoDbTodoUpdated _todoUpdatedTodo _todoUpdatedEvent

getAllLatestTodoTrashed ::
  ( Database be TomadoDb,
    HasTableEquality be EventT,
    HasSqlEqualityCheck be Int32
  ) =>
  Q be TomadoDb s (TodoTrashedT (QExpr be s), EventT (QExpr be s))
getAllLatestTodoTrashed = getAllLatestTodoEvents _tomadoDbTodoTrashed _todoTrashedTodo _todoTrashedEvent

getAllLatestTodoEvents ::
  ( Database be TomadoDb,
    Table t,
    HasTableEquality be EventT,
    HasSqlEqualityCheck be Int32
  ) =>
  (forall (f :: Type -> Type). TomadoDb f -> f (TableEntity t)) ->
  (forall (f :: Type -> Type). t f -> PrimaryKey TodoT f) ->
  (forall (f :: Type -> Type). t f -> PrimaryKey EventT f) ->
  Q be TomadoDb s (t (QExpr be s), EventT (QExpr be s))
getAllLatestTodoEvents tableKey todoKey eventKey = do
  (_, eid) <- aggregate_ (\(u, e) -> (group_ $ todoKey u, max_ (_eventId e))) $ do
    e <- all_ (_tomadoDbEvents tomadoDb)
    u <- oneToMany_ (tableKey tomadoDb) eventKey e
    pure (u, e)
  e <- join_' (_tomadoDbEvents tomadoDb) (\e -> just_ (_eventId e) ==?. eid)
  upd <- oneToMany_ (tableKey tomadoDb) eventKey e
  pure (upd, e)
