module World where

import Mud
import MudTypes

mkWorld :: Mud ()
mkWorld = do
  nw <- mkRoom "Northwest corner"
  ne <- mkRoom "Northeast corner"
  se <- mkRoom "Southeast corner"
  sw <- mkRoom "Southwest corner"
  mkExits nw [(east, ne), (south, sw)]
  mkExits ne [(west, nw), (south, se)]
  mkExits se [(north, ne), (west, sw)]
  mkExits sw [(north, nw), (east, se)]
  installVerbs

installVerbs :: Mud ()
installVerbs = do
  mkLoneVerb "l" look
  mkLoneVerb "look" look
  mkLoneVerb "n" (flip move "north")
  mkLoneVerb "e" (flip move "east")
  mkLoneVerb "s" (flip move "south")
  mkLoneVerb "w" (flip move "west")
