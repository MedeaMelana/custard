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

type Message = (Id Player, String)

data MudState = MudState
  { mPlayers_   :: IdSet Player
  , mRooms_     :: IdSet Room
  , mIds_       :: [Int]
  , mMessages_  :: [Message]
  , mVerbs_     :: M.Map String (String -> Mud ())
  }

emptyMud :: MudState
emptyMud = MudState IM.empty IM.empty [0..] [] M.empty

data Player = Player
  { pName_    :: Maybe String
  , pRoom_    :: Maybe (Id Room)
  , pContext_ :: Context
  }

data Room = Room
  { rId_        :: Id Room
  , rName_      :: String
  , rDesc_      :: String
  , rExits_     :: M.Map String (Id Room)
  }

data Context = Context
  { cPrompt_  :: Mud String
  , cExecute_ :: String -> Mud ()
  }

$( deriveAccessors ''MudState )
$( deriveAccessors ''Player )
$( deriveAccessors ''Room )
$( deriveAccessors ''Context )
