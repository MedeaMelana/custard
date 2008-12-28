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
import With

nextId :: Mud (Id a)
nextId = do
  i:_ <- getA mIds
  mIds %: tail
  return i

newPlayer :: Mud (Id Player)
newPlayer = do
  pid <- nextId
  let player = Player Nothing Nothing (loginContext pid)
  mPlayers %: (IM.insert pid player)
  tellLn pid "Welcome to Custard!"
  return pid

loginContext :: Id Player -> Context
loginContext p = Context
  { cPrompt_  = return "By what name do you wish to be known? "
  , cExecute_ = \name -> do
      ps <- getA mPlayers
      let taken = any (maybe False (== name) . pName_) (IM.elems ps)
      case (nameOkay name, taken) of
        (False, _)  -> tellLn p "Sorry, that is not a valid name."
        (_, True)   -> tellLn p "Sorry, that name is already taken."
        _           -> do
          room <- defaultRoom
          mPlayers .> byId p .> pName     %= Just name
          mPlayers .> byId p .> pRoom     %= Just room
          mPlayers .> byId p .> pContext  %= playContext p
          look p
  }

playContext :: Id Player -> Context
playContext p = Context
  { cPrompt_  = return "> "
  , cExecute_ = \cmd -> do
      cmds <- collectVerbs p
      case splitCommand cmd of
        Nothing           -> return ()
        Just (verb, args) ->
          case M.lookup verb cmds of
            Nothing     -> tellLn p "Unrecognised command."
            Just action -> action p args
  }

-- | Yield the room players end up in right after logging in.
defaultRoom :: Mud (Id Room)
defaultRoom = liftM (head . IM.keys) (getA mRooms)

mkRoom :: String -> Mud (Id Room)
mkRoom name = do
  rid <- nextId
  let room = Room name "" M.empty
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

prompt :: Id Player -> Mud ()
prompt p = do
  computePrompt <- getA (mPlayers .> byId p .> pContext .> cPrompt)
  computePrompt >>= tell p

execute :: Id Player -> String -> Mud ()
execute p cmd = do
  exec <- getA (mPlayers .> byId p .> pContext .> cExecute)
  exec cmd

-- | Yields the union of the global commands and the room-specific commands.
collectVerbs :: Id Player -> Mud (M.Map String (Id Player -> String -> Mud ()))
collectVerbs p = do
  global <- getA mVerbs
  Just room   <- getA (mPlayers .> byId p .> pRoom)
  exits  <- exitVerbs room
  return (M.union exits global)

-- | Yields the commands in a specific room, including the exits.
exitVerbs :: Id Room -> Mud Verbs
exitVerbs r = liftM toCommands $ getA (mRooms .> byId r .> rExits)
  where toCommands = M.mapWithKey (\exitName _ player _ -> move player exitName)

move :: Id Player -> String -> Mud ()
move p exit = do
  Just fromId <- getA (mPlayers .> byId p .> pRoom)
  mDestId     <- liftM (M.lookup exit) $ getA (mRooms .> byId fromId .> rExits)
  Just pname  <- getA (mPlayers .> byId p .> pName)
  case mDestId of
    Nothing -> tellLn p "You cannot go in that direction."
    Just destId -> do
      sayLn fromId (pname ++ " leaves " ++ exit ++ ".")
      mPlayers .> byId p .> pRoom %= Just destId
      sayLn destId (pname ++ " enters.")
      look p

-- | Send a message to a player.
tell :: Id Player -> String -> Mud ()
tell p m = mMessages %: (++ [(p, m)])

tellLn :: Id Player -> String -> Mud ()
tellLn p m = tell p (m ++ "\n")

-- | Send a message to all players in a room.
say :: Id Room -> String -> Mud ()
say r m = playersInRoom r >>= mapM_ (\p -> tell p m)

sayLn :: Id Room -> String -> Mud ()
sayLn r m = say r (m ++ "\n")

-- | Yield all players in a room.
playersInRoom :: Id Room -> Mud [Id Player]
playersInRoom room = do
  pmap <- getA mPlayers
  let inRoom = (== Just room) . (^. pRoom) . (pmap IM.!)
  return . filter inRoom . IM.keys $ pmap

-- | Tells whether a player is in the given room.
inRoom :: Id Player -> Id Room -> Mud Bool
inRoom p r = liftM (== Just r) $ getA (mPlayers .> byId p .> pRoom)

-- | Yields all collected messages and empties the buffer.
flushMessages :: Mud [Message]
flushMessages = do
  ms <- getA mMessages
  mMessages %= []
  return ms

look :: Id Player -> Mud ()
look p = do
  mr <- getA (mPlayers .> byId p .> pRoom)
  case mr of
    Nothing -> tellLn p "All around you is thick darkness, as far as the eye can see."
    Just r  -> getA (mRooms .> byId r .> rName) >>= tellLn p

-- | Installs a verb.
mkVerb :: String -> (Id Player -> String -> Mud ()) -> Mud ()
mkVerb verb action = mVerbs %: M.insert verb action

-- | Installs a verb that ignores its arguments.
mkLoneVerb :: String -> (Id Player -> Mud ()) -> Mud ()
mkLoneVerb verb action = mkVerb verb (const . action)
