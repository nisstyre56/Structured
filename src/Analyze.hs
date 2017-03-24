{-# LANGUAGE OverloadedStrings #-}

module Analyze () where

import qualified Filesystem.Path.CurrentOS as FP
import Parse.Python

getDependencies :: FP.FilePath -> [FP.FilePath]
getDependencies = undefined
