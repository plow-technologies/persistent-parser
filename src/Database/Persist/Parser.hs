{-|
Module      : Database.Persist.Parser
Description : Persistent model file parsing functions
Copyright   : (c) James M.C. Haver II
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta
-}

module Database.Persist.Parser (
    parseModelsFile
  , parseQuasiQuotersFile
  ) where

import Database.Persist.Internal.Parser
