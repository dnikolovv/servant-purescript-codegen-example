{-# LANGUAGE TypeApplications #-}

module Main where

import System.Environment
import System.IO (print)
import GenTypesDemo.API.CodeGen
import RIO
import RIO.List

main :: IO ()
main = do
  args <- getArgs
  case headMaybe args of
    Just path -> genPureScriptTypes path >> genServant path
    Nothing -> print @String "Missing arguments."
