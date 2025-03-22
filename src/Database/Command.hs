{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Command
  ( createEvent,
    createTodo,
    createTodoCreated,
    createTodoUpdated,
    createTodoTrashed,
  )
where

import Data.TodoEntity (TodoEntity, TodoEntityT (..), todoId)
import qualified Data.TodoEntity as TE (TodoId (TodoId))
import Database.Beam
import Database.Beam.Backend
import Database.Model.Event (Event, EventParentId, EventT (Event))
import Database.Model.Event.TodoCreated (TodoCreatedT (TodoCreated))
import Database.Model.Event.TodoTrashed (TodoTrashedT (TodoTrashed))
import Database.Model.Event.TodoUpdated (TodoUpdatedT (..))
import Database.Model.Todo (PrimaryKey (TodoId), Todo, TodoId, TodoT (Todo))
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
              _todoUpdatedPriority = val_ $ fromIntegral <$> todoPriority upd,
              _todoUpdatedDueDate = val_ $ todoDueDate upd,
              _todoUpdatedParent = val_ $ maybe nothing_ (just_ . transform) (todoParent upd)
            }
        ]

createTodoTrashed ::
  ( HasQBuilder be,
    MonadBeam be m
  ) =>
  TE.TodoId ->
  Bool ->
  Event ->
  m ()
createTodoTrashed tid isTrashed ev =
  runInsert $
    insert (_tomadoDbTodoTrashed tomadoDb) $
      insertExpressions [TodoTrashed default_ (val_ $ primaryKey ev) (val_ $ transform tid) (val_ isTrashed)]

transform :: TE.TodoId -> TodoId
transform (TE.TodoId i) = TodoId (fromIntegral i)
