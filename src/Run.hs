{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Tomado

run :: AppM App ()
run = do
  logInfo "We're inside the application!"
