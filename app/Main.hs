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
            [ command "init" $ info initOpt (progDesc "setup TomaDo working space"),
              command "config" $ info configOpt (progDesc "display or edit TomaDo configuration"),
              command "list" $ info listTodoOpt (progDesc "display a todo list"),
              command "today" $ info todayTodoOpt (progDesc "display today's todo list"),
              command "add" $ info addTodoOpt (progDesc "make a new todo"),
              command "done" $ info doneTodoOpt (progDesc "make a todo done")
            ]
      pure Options {..}
    initOpt = pure (Init InitOpt)
    configOpt = pure (Config ConfigOpt)
    listTodoOpt = pure (ListTodo ListTodoOpt)
    todayTodoOpt = pure (ListTodo ListTodoOpt)
    addTodoOpt = do
      addTodoDescription <- argument str (metavar "\"{the To-Do Description}\"")
      addTodoPriority <-
        option auto
          $ fold
            [ long "priority",
              short 'p',
              metavar "INT",
              value 0,
              showDefault,
              help "Priority of the To-Do"
            ]
      addTodoDueDate <- optional $ option auto (long "due" <> short 'd' <> metavar "DATE" <> help "Due date of the To-Do")
      pure (AddTodo AddTodoOpt {..})
    doneTodoOpt = pure (UpdateTodo UpdateTodoOpt)

main :: IO ()
main = run =<< execParser options
