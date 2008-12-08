{-# OPTIONS -fglasgow-exts #-}

module Network.Custard.World where

import Network.Custard.Core

mkWorld :: Mud ()
mkWorld = mdo
  nw <- mkRoom "Northwest corner" [("e", ne), ("s", sw)]
  ne <- mkRoom "Northeast corner" [("w", nw), ("s", se)]
  se <- mkRoom "Southeast corner" [("n", ne), ("w", sw)]
  sw <- mkRoom "Southwest corner" [("n", nw), ("e", se)]
  return ()
