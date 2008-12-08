module Main where

import Network.Custard.Engine
import Network.Custard.World
import System.Environment

main :: IO ()
main = do
  [port] <- getArgs
  runCustard (read port) mkWorld
