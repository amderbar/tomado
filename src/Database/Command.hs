{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Command where

import Data.TodoEntity (TodoEntity, TodoEntityT (..), todoId)
import qualified Data.TodoEntity as TE (TodoId (TodoId))
import Database.Beam
import Database.Beam.Backend
import Database.Model.Event (Event, EventParentId, EventT (Event))
import Database.Model.Event.TodoCreated (TodoCreatedT (TodoCreated))
import Database.Model.Event.TodoUpdated (TodoUpdatedT (..))
import Database.Model.Todo (PrimaryKey (TodoId), Todo, TodoId, TodoParentId, TodoT (Todo), nullTodoId)
import Database.Schema (TomadoDb (..), tomadoDb)
import Import (Int32, Text)
import RIO.Time (LocalTime)

type HasSqlValueSyntax' be a = HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92UpdateExpressionSyntax (Sql92UpdateSyntax (BeamSqlBackendSyntax be)))) a

createEvent ::
  ( BeamSqlBackend be,
    HasSqlValueSyntax' be (Maybe Int32)
  ) =>
  EventParentId ->
  SqlInsert be EventT
createEvent parentEventId =
  insert (_tomadoDbEvents tomadoDb) $
    insertExpressions [Event default_ (val_ parentEventId) currentTimestamp_]

createTodo :: (BeamSqlBackend be) => SqlInsert be TodoT
createTodo = insert (_tomadoDbTodo tomadoDb) $ insertExpressions [Todo default_]

createTodoCreated ::
  ( HasQBuilder be,
    HasSqlValueSyntax' be Text,
    MonadBeam be m
  ) =>
  Text ->
  Todo ->
  Event ->
  m ()
createTodoCreated desc ntd ev =
  runInsert $
    insert (_tomadoDbTodoCreated tomadoDb) $
      insertExpressions [TodoCreated default_ (val_ $ primaryKey ev) (val_ $ primaryKey ntd) (val_ desc)]

createTodoUpdated ::
  ( HasQBuilder be,
    HasSqlValueSyntax' be (Maybe Int32),
    HasSqlValueSyntax' be Text,
    HasSqlValueSyntax' be (Maybe LocalTime),
    MonadBeam be m
  ) =>
  TodoEntity ->
  Event ->
  m ()
createTodoUpdated upd ev =
  runInsert $
    insert (_tomadoDbTodoUpdated tomadoDb) $
      insertExpressions
        [ TodoUpdated
            { _todoUpdatedId = default_,
              _todoUpdatedEvent = val_ $ primaryKey ev,
              _todoUpdatedTodo = val_ $ transform (todoId upd),
              _todoUpdatedDescription = val_ $ todoDescription upd,
              _todoUpdatedDetail = val_ $ todoDetail upd,
              _todoUpdatedDone = val_ $ todoDone upd,
              _todoUpdatedPriority = val_ $ todoPriority upd,
              _todoUpdatedDueDate = val_ $ todoDueDate upd,
              _todoUpdatedParent = val_ $ transform' (todoParent upd)
            }
        ]
  where
    transform :: TE.TodoId -> TodoId
    transform (TE.TodoId i) = TodoId i

    transform' :: Maybe TE.TodoId -> TodoParentId
    transform' (Just (TE.TodoId i)) = TodoId (Just i)
    transform' Nothing = nullTodoId
