{-# LANGUAGE RecursiveDo #-}

module Mud where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Char as C
import Data.Maybe (catMaybes, listToMaybe)
import Data.Accessor
import Control.Monad
import Control.Applicative
import MudTypes
import Text
import With

class MudEntity a where
  entityAccessor :: Accessor MudState (IdSet a)

instance MudEntity Room where
  entityAccessor = mRooms

instance MudEntity Player where
  entityAccessor = mPlayers

instance MudEntity Object where
  entityAccessor = mObjects

selectById :: MudEntity a => Id a -> Mud a
selectById i = getA (entityAccessor .> byId i)

create :: MudEntity a => a -> Mud (Id a)
create x = do
  xid <- nextId
  entityAccessor %: IM.insert xid x
  return xid

select :: MudEntity a => (a -> Bool) -> Mud [I a]
select p = filter (p . snd) . IM.toList <$> getA entityAccessor

update :: MudEntity a => Id a -> (a -> a) -> Mud ()
update i f = entityAccessor .> byId i %: f

nextId :: Mud (Id a)
nextId = do
  i:_ <- getA mIds
  mIds %: tail
  return i

newPlayer :: Mud (Id Player)
newPlayer = mdo
  pid <- create (Player Nothing Nothing (loginContext pid))
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
          mroom <- defaultRoom
          with (mPlayers .> byId p) $ do
            pName     %= Just name
            pContext  %= playContext p
          case mroom of
            Nothing   -> look p
            Just room -> appear room p
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
defaultRoom :: Mud (Maybe (Id Room))
defaultRoom = liftM (listToMaybe . IM.keys) (getA mRooms)

mkRoom :: String -> Mud (Id Room)
mkRoom name = do
  rid <- nextId
  let room = Room name "" M.empty
  mRooms %: IM.insert rid room
  return rid

mkExits :: Id Room -> [(String, Exit)] -> Mud ()
mkExits r = sequence_ . map (mkExit r)

mkExit :: Id Room -> (String, Exit) -> Mud ()
mkExit r (ex, er) = mRooms .> byId r .> rExits %: M.insert ex er

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

appear :: Id Room -> Id Player -> Mud ()
appear r p = do
  mPlayers .> byId p .> pRoom %= Just r
  Just name <- getA (mPlayers .> byId p .> pName)
  sayLn r (/= p) (name ++ " suddenly appears right beside you!")
  look p

-- | Send a message to a player.
tell :: Id Player -> String -> Mud ()
tell p m = addEffect (Message p m)

-- | Registers a side effect to be executed by the server.
addEffect :: Effect -> Mud ()
addEffect eff = mEffects %: (++ [eff])

-- | Calls tell with a newline appended to the message.
tellLn :: Id Player -> String -> Mud ()
tellLn p m = tell p (m ++ "\n")

-- | Send a message to all players in a room that satisfy the condition.
say :: Id Room -> (Id Player -> Bool) -> String -> Mud ()
say r ok m = playersInRoom r >>= mapM_ (\p -> when (ok p) $ tell p m)

-- | Calls say with a newline appended to the message.
sayLn :: Id Room -> (Id Player -> Bool) -> String -> Mud ()
sayLn r ok m = say r ok (m ++ "\n")

-- | Send a message to all players that satisfy the condition.
shout :: (Id Player -> Bool) -> String -> Mud ()
shout ok m = do
  ps <- getA mPlayers
  forM_ (filter ok $ IM.keys ps) $ \p -> tell p m

-- | Calls shout with a newline appended to the message.
shoutLn :: (Id Player -> Bool) -> String -> Mud ()
shoutLn ok m = shout ok (m ++ "\n")

-- | Yield all players in a room.
playersInRoom :: Id Room -> Mud [Id Player]
playersInRoom room = do
  pmap <- getA mPlayers
  let inRoom = (== Just room) . (^. pRoom) . (pmap IM.!)
  return . filter inRoom . IM.keys $ pmap

-- -- | Tells whether a player is in the given room.
-- inRoom :: Id Player -> Id Room -> Mud Bool
-- inRoom p r = liftM (== Just r) $ getA (mPlayers .> byId p .> pRoom)

-- | Yields all collected messages and empties the buffer.
flushEffects :: Mud [Effect]
flushEffects = do
  es <- getA mEffects
  mEffects %= []
  return es

-- | Installs a verb.
mkVerb :: String -> Verb -> Mud ()
mkVerb verb action = mVerbs %: M.insert verb action

-- | Installs a verb that ignores its arguments.
mkSimpleVerb :: String -> (Id Player -> Mud ()) -> Mud ()
mkSimpleVerb verb action = mkVerb verb (const action)

move :: String -> Id Player -> Mud ()
move exitName p = do
  Just fromId <- getA (mPlayers .> byId p .> pRoom)
  mExit       <- M.lookup exitName `liftM` getA (mRooms .> byId fromId .> rExits)
  Just pname  <- getA (mPlayers .> byId p .> pName)
  case mExit of
    Nothing -> tellLn p "You cannot go in that direction."
    Just exit -> do
      let destId = exit ^. eTarget
      tellLn p (exit ^. e1PGo)
      sayLn fromId (/= p) (exit ^. e3PLeave $ pname)
      sayLn destId (/= p) (exit ^. e3PEnter $ pname)
      mPlayers .> byId p .> pRoom %= Just destId
      look p

-- | Describe a player's surroundings to them.
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
      
      -- Objects lying around.
      objs <- select (inRoom r)
      unless (null objs) $ tellLn p $ "Lying on the floor: " ++ listify (map (oNoun_ . snd) objs) ++ "."

      -- Players in room.
      ps <- filter (/= p) <$> playersInRoom r
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

-- | Causes the player to logoff.
quit :: Id Player -> Mud ()
quit p = do
  Just name <- getA (mPlayers .> byId p .> pName)
  Just room <- getA (mPlayers .> byId p .> pRoom)
  tellLn p "Thank you for playing!"
  sayLn room (/= p) (name ++ " vanishes in a puff of smoke.")
  addEffect (Logoff p)

-- | Removes the player from the game, cleaning any pointers to it.
--   This action produces no side effects.
doQuit :: Id Player -> Mud ()
doQuit p = mPlayers %: IM.delete p

disconnected :: Id Player -> Mud ()
disconnected p = do
  mname <- getA (mPlayers .> byId p .> pName)
  mroom <- getA (mPlayers .> byId p .> pRoom)
  case (mname, mroom) of
    (Just name, Just room)  -> sayLn room (/= p) (name ++ " vanishes in a puff of smoke.")
    _                       -> return ()
  addEffect (Logoff p)

playerByName :: String -> Mud (Maybe (Id Player))
playerByName name = do
  let lowerName = map C.toLower name
  ps <- IM.toList `fmap` getA mPlayers
  case [ pid | (pid, p) <- ps, Just lowerName == fmap C.toLower `fmap` (p ^. pName) ] of
    pid : _ -> return (Just pid)
    []    -> return Nothing

class IsLocation loc where
  toLocation :: loc -> Location
  