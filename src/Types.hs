{-# LANGUAGE NoImplicitPrelude #-}
module Types
  ( App (..)
  , Options (..)
  ) where

import RIO
import RIO.Process
import Database.SQLite.Simple (Connection)

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  , appDbConnection :: !Connection
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
