module Network.Custard.Core where

import Control.Concurrent
import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Map as M
import Network.Custard.Util

data Player = Player
  { pName :: String
  , pRoom :: MVar Room
  , pWrite :: String -> IO ()
  , pGetLine :: IO String
  }

instance Eq Player where
  p1 == p2 = pName p1 == pName p2

instance Ord Player where
  compare p1 p2 = compare (pName p1) (pName p2)

pWriteLn :: Player -> String -> IO ()
pWriteLn p s = pWrite p s >> pWrite p "\n"

-- | Asks the player a question repeatedly, until the predicate is satisfied.
query :: Player -> (String -> Bool)  -> String -> IO String
query p ok prompt = loop where
  loop = do
    pWrite p prompt
    line <- pGetLine p
    if ok line
      then return (trim line)
      else loop

data Room = Room 
  { rName :: String
  , rPlayers :: MVar (S.Set Player)
  , rExits :: M.Map String Room
  }

type Mud = StateT MudState IO

data MudState = MudState
  { mRooms :: [Room]
  , mPlayers :: [Player]
  }

emptyMud :: MudState
emptyMud = MudState [] []

mkRoom :: String -> [(String, Room)] -> Mud Room
mkRoom name exits = do
  vPlayers <- liftIO $ newMVar S.empty
  let room = Room name vPlayers (M.fromList exits)
  modify (\s -> s { mRooms = room : mRooms s })
  return room
