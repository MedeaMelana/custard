module Mud where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Char as C
import Data.Maybe (catMaybes)
import Data.Accessor
import Control.Monad.State
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
          with (mPlayers .> byId p) $ do
            pName     %= Just name
            pContext  %= playContext p
          appear room p
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
            Just action -> action args p
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
collectVerbs :: Id Player -> Mud Verbs
collectVerbs p = do
  global <- getA mVerbs
  Just room   <- getA (mPlayers .> byId p .> pRoom)
  exits  <- exitVerbs room
  return (M.union exits global)

-- | Yields the verbs in a specific room, including the exits.
exitVerbs :: Id Room -> Mud Verbs
exitVerbs r = liftM toCommands $ getA (mRooms .> byId r .> rExits)
  where toCommands = M.mapWithKey (\exitName _ _ player -> move exitName player)

move :: String -> Id Player -> Mud ()
move exit p = do
  Just fromId <- getA (mPlayers .> byId p .> pRoom)
  mDestId     <- liftM (M.lookup exit) $ getA (mRooms .> byId fromId .> rExits)
  Just pname  <- getA (mPlayers .> byId p .> pName)
  case mDestId of
    Nothing -> tellLn p "You cannot go in that direction."
    Just destId -> do
      sayLn fromId (/= p) (pname ++ " leaves " ++ exit ++ ".")
      mPlayers .> byId p .> pRoom %= Just destId
      sayLn destId (/= p) (pname ++ " enters.")
      look p

appear :: Id Room -> Id Player -> Mud ()
appear r p = do
  mPlayers .> byId p .> pRoom %= Just r
  Just name <- getA (mPlayers .> byId p .> pName)
  sayLn r (/= p) (name ++ " suddenly appears right beside you!")

-- | Send a message to a player.
tell :: Id Player -> String -> Mud ()
tell p m = mMessages %: (++ [(p, m)])

-- | Calls tell with a newline appended to the message.
tellLn :: Id Player -> String -> Mud ()
tellLn p m = tell p (m ++ "\n")

-- | Send a message to all players in a room that satisfy the condition.
say :: Id Room -> (Id Player -> Bool) -> String -> Mud ()
say r ok m = playersInRoom r >>= mapM_ (\p -> when (ok p) $ tell p m)

-- | Calls say with a newline appended to the message.
sayLn :: Id Room -> (Id Player -> Bool) -> String -> Mud ()
sayLn r ok m = say r ok (m ++ "\n")

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
    Nothing ->
      tellLn p "All around you is thick darkness, as far as the eye can see."
    Just r  -> do
      room <- getA (mRooms .> byId r)

      -- Room name.
      tellLn p (room ^. rName)

      -- Players in room.
      ps <- liftM (filter (/= p)) (playersInRoom r)
      playerNames <- liftM catMaybes $ forM ps $ \q -> getA (mPlayers .> byId q .> pName)
      case playerNames of
        []  -> return ()
        [q] -> tellLn p (q ++ " is here.")
        _   -> tellLn p (listify playerNames ++ " are here.")

      -- Exits.
      exitNames <- liftM M.keys $ getA (mRooms .> byId r .> rExits)
      case exitNames of
        []  -> return ()
        _   -> tellLn p ("Exits: " ++ listify exitNames ++ ".")

-- | Installs a verb.
mkVerb :: String -> Verb -> Mud ()
mkVerb verb action = mVerbs %: M.insert verb action

-- | Installs a verb that ignores its arguments.
mkLoneVerb :: String -> (Id Player -> Mud ()) -> Mud ()
mkLoneVerb verb action = mkVerb verb (const action)

playerSay :: Verb
playerSay msg p = do
  let trimmed = trim msg
  Just room <- getA (mPlayers .> byId p .> pRoom)
  Just name <- getA (mPlayers .> byId p .> pName)
  sayLn room (/= p) (name ++ " says: " ++ trimmed)
  tellLn p ("You say: " ++ trimmed)
