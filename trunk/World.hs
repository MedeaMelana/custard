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
  mkLoneVerb "n" (move "north")
  mkLoneVerb "e" (move "east")
  mkLoneVerb "s" (move "south")
  mkLoneVerb "w" (move "west")
  mkLoneVerb "u" (move "up")
  mkLoneVerb "d" (move "down")
  mkVerb "say" playerSay
  mkVerb "'" playerSay
