{-# OPTIONS -XTemplateHaskell #-}

module MudTypes where

import Data.Accessor
import Data.Accessor.Template
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Control.Monad.State as S

type Mud = S.State MudState

-- | The a is a phantom type and only serves as documentation.
type Id a = Int

type IdSet a = IM.IntMap a

byId :: Id a -> Accessor (IdSet a) a
byId key = accessor (IM.! key) (IM.insert key)

data Effect
  = Message (Id Player) String  -- ^ Send a message to a player's terminal.
  | Logoff (Id Player)          -- ^ Signals that the player is no longer referred to in the game.
  deriving (Eq, Show)

type Verb = String -> Id Player -> Mud ()  -- args -> issuer -> -> action
type Verbs = M.Map String Verb  -- verb name -> verb

data MudState = MudState
  { mPlayers_ :: IdSet Player
  , mRooms_   :: IdSet Room
  , mIds_     :: [Int]
  , mEffects_ :: [Effect]
  , mVerbs_   :: Verbs
  }

emptyMud :: MudState
emptyMud = MudState IM.empty IM.empty [0..] [] M.empty

data Player = Player
  { pName_    :: Maybe String
  , pRoom_    :: Maybe (Id Room)
  , pContext_ :: Context
  }

-- | Rooms are physical locations players will occupy.
data Room = Room
  { rName_      :: String             -- ^ Short title.
  , rDesc_      :: String             -- ^ Longer description.
  , rExits_     :: M.Map String Exit  -- ^ Maps from exit name to exit descriptor.
  }

-- | Exits allow players to walk from one room to another by entering the exit's name.
data Exit = Exit
  { eTarget_  :: Id Room          -- ^ Where the exit leads to.
  , e1PGo_    :: String           -- ^ What the player sees when moving.
  , e3PLeave_ :: String -> String -- ^ What others in the source room see. Arg: player name.
  , e3PEnter_ :: String -> String -- ^ What others in the target room see. Arg: player name.
  }

-- | A context determines a player's prompt and how their input is handled.
--   Examples: login, normal play, reading a paged piece of text, in editor.
data Context = Context
  { cPrompt_  :: Mud String       -- ^ Computes the prompt to show.
  , cExecute_ :: String -> Mud () -- ^ Executes the player's input.
  }

$( deriveAccessors ''MudState )
$( deriveAccessors ''Player )
$( deriveAccessors ''Room )
$( deriveAccessors ''Exit )
$( deriveAccessors ''Context )
