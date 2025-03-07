{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Development.Placeholders (notImplemented)
import Import
import Options.Applicative.Simple
import qualified Paths_tomado
import RIO.Process
import Run

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_tomado.version)
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
      )
      subcommands
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app run
  where
    subcommands = do
      addCommand "init" "setup TomaDo working space" $notImplemented (pure ())
      addCommand "config" "display or edit TomaDo configuration" $notImplemented (pure ())
      addCommand "list" "display a todo list" $notImplemented (pure ())
      addCommand "today" "display today's todo list" $notImplemented (pure ())
      addCommand "new" "make a new todo" $notImplemented (pure ())
      addCommand "done" "make a todo done" $notImplemented (pure ())
