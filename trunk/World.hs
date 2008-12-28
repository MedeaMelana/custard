module World where

import Mud
import MudTypes
import Verbs

mkWorld :: Mud ()
mkWorld = do
  installVerbs
  
  garden <- mkRoom "A lush garden"
  living <- mkRoom "A warm living room"
  kitchen <- mkRoom "A clean kitchen"
  bedroom <- mkRoom "A cozy bedroom"

  mkExits garden [(north, living)]
  mkExits living [(south, garden), (east, kitchen), (up, bedroom)]
  mkExits kitchen [(west, living)]
  mkExits bedroom [(down, living)]
