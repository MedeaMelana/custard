module Mud where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Char as C
import Data.Accessor
import Data.Accessor.Template
import Control.Monad.State
import Text.Regex.Posix
import MudTypes
import Text

nextId :: Mud (Id a)
nextId = do
  i:_ <- getA mIds
  mIds %: tail
  return i

newPlayer :: Mud (Id Player)
newPlayer = do
  pid <- nextId
  let player = Player (error "player name not set") (error "player room not set")
  mPlayers %: (IM.insert pid player)
  tellLn pid "Hello!"
  return pid

mkRoom :: String -> Mud (Id Room)
mkRoom name = do
  rid <- nextId
  let room = Room rid name "" M.empty
  mRooms %: IM.insert rid room
  return rid

mkExits :: Id Room -> [(String, Id Room)] -> Mud ()
mkExits r = sequence_ . map (mkExit r)

mkExit :: Id Room -> (String, Id Room) -> Mud ()
mkExit r (ex, er) = mRooms .> byId r .> rExits %: M.insert ex er

north = "north"
east = "east"
south = "south"
west = "west"

-- | Executes a player's command.
execute :: Id Player -> String -> Mud ()
execute p cmd = do
  cmds <- collectCommands p
  case splitCommand cmd of
    Nothing           -> return ()
    Just (verb, args) ->
      case M.lookup verb cmds of
        Nothing     -> error "unrecognized command"
        Just action -> action p args

-- | Yields the union of the global commands and the room-specific commands.
collectCommands :: Id Player -> Mud Commands
collectCommands p = do
  global <- getA mCommands
  room   <- getA (mPlayers .> byId p .> pRoom)
  exits  <- exitCommands room
  return (M.union exits global)

-- | Yields the commands in a specific room, including the exits.
exitCommands :: Id Room -> Mud Commands
exitCommands r = liftM toCommands $ getA (mRooms .> byId r .> rExits)
  where toCommands = M.mapWithKey (\exitName _ player _ -> move player exitName)

move :: Id Player -> String -> Mud ()
move p exit = do
  fromId  <- getA (mPlayers .> byId p .> pRoom)
  destId  <- liftM (M.! exit) $ getA (mRooms .> byId fromId .> rExits)
  pname   <- getA (mPlayers .> byId p .> pName)
  say fromId (pname ++ " leaves " ++ exit ++ ".")
  mPlayers .> byId p .> pRoom %= destId
  say destId (pname ++ " enters.")
  -- look p

-- | Send a message to a player.
tell :: Id Player -> String -> Mud ()
tell p m = mMessages %: (++ [(p, m)])

tellLn :: Id Player -> String -> Mud ()
tellLn p m = tell p (m ++ "\n")

-- | Send a message to all players in a room.
say :: Id Room -> String -> Mud ()
say r m = playersInRoom r >>= mapM_ (\p -> tell p m)

-- | Yield all players in a room.
playersInRoom :: Id Room -> Mud [Id Player]
playersInRoom room = do
  pmap <- getA mPlayers
  let inRoom = (== room) . (^. pRoom) . (pmap IM.!)
  return . filter inRoom . IM.keys $ pmap

-- moveEveryoneTo :: Id Room -> Mud ()
-- moveEveryoneTo r = do
--   ps <- getA mPlayers
--   forM_ (keys ps) (move r)

-- | Tells whether a player is in the given room.
inRoom :: Id Player -> Id Room -> Mud Bool
inRoom p r = liftM (== r) $ getA (mPlayers .> byId p .> pRoom)

-- | Yields all collected messages and empties the buffer.
flushMessages :: Mud [Message]
flushMessages = do
  ms <- getA mMessages
  mMessages %= []
  return ms

prompt :: Id Player -> Mud ()
prompt p = tell p "> "
