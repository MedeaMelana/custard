module Main where

import Engine
import World
import System.Environment

main :: IO ()
main = do
  port : _ <- getArgs
  runCustard (fromIntegral $ read port) mkWorld
