{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import qualified Data.Version as Ver (showVersion)
import Import
import Options.Applicative
import qualified Paths_tomado
import Run (run)

options :: ParserInfo Options
options =
  info
    (opts <**> showVersion <**> helper)
    ( fullDesc
        <> header "[x] Manage your To-Do and Work time with @TomaDo"
        <> progDesc "Program description, also for command line arguments"
    )
  where
    showVersion :: Parser (a -> a)
    showVersion =
      infoOption
        (Ver.showVersion Paths_tomado.version)
        (long "version" <> help "Show version" <> hidden)

    opts :: Parser Options
    opts = do
      optionsVerbose <- switch (long "verbose" <> short 'v' <> help "Verbose output?")
      optionsAction <-
        hsubparser
          $ fold
            [ command "config" $ info configOpt (progDesc "display or edit TomaDo configuration"),
              command "list" $ info listTodoOpt (progDesc "display a todo list"),
              command "today" $ info todayTodoOpt (progDesc "display today's todo list"),
              command "add" $ info addTodoOpt (progDesc "make a new todo"),
              command "edit" $ info editTodoOpt (progDesc "edit a todo"),
              command "done" $ info doneTodoOpt (progDesc "make a todo done. Alias of 'edit ${ID} --done'"),
              command "trash" $ info trashTodoOpt (progDesc "throw a todo to trash")
            ]
      pure Options {..}

    configOpt = pure (Configure ConfigOpt)

    listTodoOpt = pure (ListTodo ListTodoOpt)

    todayTodoOpt = pure (ListTodo ListTodoOpt)

    addTodoOpt = do
      addTodoDescription <- argument str (metavar "\"{TO-DO DESCRIPTION}\"")
      addTodoPriority <- optional optionPriority
      addTodoDueDate <- optional optionDueDate
      addTodoDetail <- switch (long "detail" <> help "Set detail of the To-Do form stdin")
      pure (AddTodo AddTodoOpt {..})

    editTodoOpt = do
      updateTodoId <- argument auto (metavar "ID")
      updateTodoDescription <- optional optionDescription
      updateTodoPriority <- optional optionPriority
      updateTodoDueDate <- optional optionDueDate
      updateTodoDone <- optional (optionDone <|> optionUnDone)
      updateTodoDetail <- switch (long "detail" <> help "Open editor to edit detail of the To-Do")
      pure (UpdateTodo UpdateTodoOpt {..})

    doneTodoOpt = do
      opt <- emptyUpdateTodoOpt <$> argument auto (metavar "ID")
      pure (UpdateTodo opt {updateTodoDone = Just True})

    trashTodoOpt = do
      trashTodoId <- argument auto (metavar "ID")
      pure (TrashTodo TrashTodoOpt {..})

    optionDescription :: Parser Text
    optionDescription =
      strOption
        $ fold
          [ long "desc",
            metavar "DESCRIPTION",
            help "set Description of the To-Do"
          ]

    optionPriority :: Parser Int
    optionPriority =
      option auto
        $ fold
          [ long "priority",
            metavar "INT",
            help "set Priority of the To-Do"
          ]

    optionDueDate :: Parser LocalTime
    optionDueDate =
      option auto
        $ fold
          [ long "due",
            metavar "DATE",
            help "set Due date of the To-Do"
          ]

    optionDone :: Parser Bool
    optionDone = flag' True (long "done" <> help "Mark the To-Do as done")

    optionUnDone :: Parser Bool
    optionUnDone = flag' False (long "undone" <> help "Mark the To-Do as not done")

main :: IO ()
main = run =<< execParser options
