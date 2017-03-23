{-# LANGUAGE OverloadedStrings #-}
module Main where

import IndexCode
import Control.Applicative
import System.Environment

main = do
        args <- getArgs
        files <- getFiles $ head args
        mapM_ print files
