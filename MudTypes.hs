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
  = Message (Id Player) String
  | Logoff (Id Player)
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

data Room = Room
  { rName_      :: String
  , rDesc_      :: String
  , rExits_     :: M.Map String (Id Room)
  } deriving (Show, Eq)

-- | A context determines a player's prompt and how their input is handled.
--   Examples: login, normal play, reading a paged piece of text, in editor.
data Context = Context
  { cPrompt_  :: Mud String       -- ^ Computes the prompt to show.
  , cExecute_ :: String -> Mud () -- ^ Executes the player's input.
  }

$( deriveAccessors ''MudState )
$( deriveAccessors ''Player )
$( deriveAccessors ''Room )
$( deriveAccessors ''Context )
