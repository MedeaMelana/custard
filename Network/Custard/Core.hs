module Network.Custard.Core where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Map as M
import Network.Custard.Util

data Player = Player
  { pName :: String
  , pRoom :: Room
  , pChan :: Chan SCMessage
  }

instance Eq Player where
  p1 == p2 = pName p1 == pName p2

instance Ord Player where
  compare p1 p2 = compare (pName p1) (pName p2)

type PlayerId = String -- should be something more unique

-- Messages from the client to the server.
data CSMessage
-- Send client input to the server.
    = Input PlayerId String
-- The client has died. There is no point in communicating any further.
    | Died PlayerId
-- The client requests to register as a new player.
    | NewPlayer PlayerId (Chan SCMessage)

-- Messages from the server to the client.
data SCMessage
-- Show this message on the client. No response is expected.
    = Message String
-- Ask the client a modal question. Appropriate responses are Input and Died.
    | Ask String
-- Irrevocably kill the client. Should be accepted at all times. The server
-- will immediately stop listening.
    | Kill
-- Signal that the previous request of the client is accepted and processed.
    | ServerAck


pWrite :: Player -> SCMessage -> Mud ()
pWrite p = liftIO . writeChan (pChan p)

pWriteLn :: Player -> String -> Mud()
pWriteLn p = pWrite p . Message . (++ "\n")

{- 
pWrite':: Player -> String -> IO ()
pWrite' p s = writeChan (pChan p) (Message s)
pWrite :: Player -> String -> Mud ()
pWrite p s = liftIO $ pWrite' p s

pWriteLn':: Player -> String -> IO ()
pWriteLn' p s = writeChan (pChan p) (Message $ s ++ "\n")
pWriteLn :: Player -> String -> Mud ()
pWriteLn p s = liftIO $ pWriteLn' p s

pGetLine :: Player -> Mud String
pGetLine p = undefined $ do
  state <- get
  msg <- liftIO $ readChan (mChan state)
  case msg of
    Input _ str -> return str
    _           -> error "No input"

-- | Asks the player a question repeatedly, until the predicate is satisfied.
query :: Player -> (String -> Bool)  -> String -> Mud String
query p ok prompt = loop where
  loop = do
    pWrite p prompt
    line <- pGetLine p
    if ok line
      then return (trim line)
      else loop
-}

data Room = Room 
  { rName    :: String
  , rPlayers :: S.Set Player
  , rExits   :: M.Map String Room
  }

type Mud = StateT MudState IO

data MudState = MudState
  { mRooms   :: [Room]
  , mPlayers :: [Player]
  , mChan    :: Chan CSMessage
  }

emptyMud :: IO MudState
emptyMud = do
  c <- newChan
  return $ MudState [] [] c

mkRoom :: String -> [(String, Room)] -> Mud Room
mkRoom name exits = do
  let room = Room name S.empty (M.fromList exits)
  modify (\s -> s { mRooms = room : mRooms s })
  return room

runMud :: Mud a -> IO a
runMud m = do
  state <- emptyMud
  (res, _) <- runStateT m state
  return res

