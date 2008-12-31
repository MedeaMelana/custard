module World where

import Mud
import MudTypes
import Verbs

mkWorld :: Mud ()
mkWorld = do
  installVerbs
  
  -- Create rooms.
  garden <- mkRoom "A lush garden"
  living <- mkRoom "A warm living room"
  kitchen <- mkRoom "A clean kitchen"
  bedroom <- mkRoom "A cozy bedroom"
  wardrobe <- mkRoom "Inside a pitch dark wardrobe"

  -- Link rooms.
  mkExits garden [north living]
  mkExits living [south garden, east kitchen, eUp bedroom]
  mkExits kitchen [west living]
  mkExits bedroom [eDown living, eWardrobe wardrobe]
  mkExits wardrobe [eOut bedroom]

mkStdExit :: String -> String -> Id Room -> (String, Exit)
mkStdExit toDir fromDir target = (toDir, Exit
  { eTarget_  = target
  , e1PGo_    = "You walk " ++ toDir ++ "."
  , e3PLeave_ = (++ " leaves " ++ toDir ++ ".")
  , e3PEnter_ = (++ " walks in from the " ++ fromDir ++ ".")
  })

north = mkStdExit "north" "south"
east  = mkStdExit "east"  "west"
south = mkStdExit "south" "north"
west  = mkStdExit "west"  "east"

eUp :: Id Room -> (String, Exit)
eUp target = ("up", Exit 
  { eTarget_  = target
  , e1PGo_    = "You climb up."
  , e3PLeave_ = (++ " leaves up.")
  , e3PEnter_ = (++ " arrives from below.")
  })

eDown :: Id Room -> (String, Exit)
eDown target = ("down", Exit 
  { eTarget_  = target
  , e1PGo_    = "You walk down."
  , e3PLeave_ = (++ " leaves down.")
  , e3PEnter_ = (++ " arrives from above.")
  })

eWardrobe :: Id Room -> (String, Exit)
eWardrobe target = ("wardrobe", Exit 
  { eTarget_  = target
  , e1PGo_    = "You open the wardrobe and climb into it."
  , e3PLeave_ = (++ " opens the wardrobe and clambers into it.")
  , e3PEnter_ = \p -> "Your surroundings are briefly illuminated as " ++ p ++ " joins you from outside."
  })

eOut :: Id Room -> (String, Exit)
eOut target = ("out", Exit
  { eTarget_  = target
  , e1PGo_    = "You climb back out of the wardrobe."
  , e3PLeave_ = (++ " climbs back out of the wardrobe.")
  , e3PEnter_ = \p -> "The wardrobe doors suddenly open and " ++ p ++ " steps out."
  })
