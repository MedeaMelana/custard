module Verbs where

import Mud
import MudTypes
import Text
import Parser
import Data.Accessor
import Control.Applicative
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Char as C
import Control.Monad
import Data.Maybe

withArg :: String -> Verb -> Verb
withArg error verb args player = do
  let trimmed = trim args
  if null trimmed
    then tellLn player error
    else verb trimmed player

playerSay :: Verb
playerSay = withArg "Say what?" $ \msg p -> do
  Just room <- getA (mPlayers .> byId p .> pRoom)
  Just name <- getA (mPlayers .> byId p .> pName)
  sayLn room (/= p) (name ++ " says: " ++ msg)
  tellLn p ("You say: " ++ msg)

chat :: Verb
chat = withArg "Chat what?" $ \msg -> chatEmote (": " ++ msg)

chatEmote :: Verb
chatEmote msg p = do
  case trim msg of
    []    -> tellLn p "Chat what?"
    c:cs  -> do
      let sep = if C.isPunctuation c then "" else " "
      Just name <- getA (mPlayers .> byId p .> pName)
      shoutLn (const True) ("(chat) " ++ name ++ sep ++ c:cs)

emote :: Verb
emote = withArg "Emote what?" $ \msg p -> do
  Just room <- getA (mPlayers .> byId p .> pRoom)
  Just name <- getA (mPlayers .> byId p .> pName)
  let line = name ++ " " ++ trim msg
  sayLn room (/= p) line
  tellLn p ("You emote: " ++ line)

mkSoul :: String -> String -> String -> Mud ()
mkSoul name first third = mkSimpleVerb name $ \p -> do
  Just room <- getA (mPlayers .> byId p .> pRoom)
  Just name <- getA (mPlayers .> byId p .> pName)
  tellLn p ("You " ++ first)
  sayLn room (/= p) (name ++ " " ++ third)

help :: SimpleVerb
help p = do
  verbs <- M.keys `fmap` getA mVerbs
  tellLn p ("Available commands: " ++ listify verbs ++ ".")

playerTell :: Verb
playerTell = parse (doTell <$> pPlayer (const True) <*> pRest)
  where doTell target msg source = do
          Just sourceName <- getA (mPlayers .> byId source .> pName)
          Just targetName <- getA (mPlayers .> byId target .> pName)
          tellLn source ("You tell " ++ targetName ++ ": " ++ msg)
          tellLn target (sourceName ++ " tells you: " ++ msg)

who :: SimpleVerb
who p = do
  ps <- IM.elems `fmap` getA mPlayers
  let names = [ name | p <- ps, let Just name = p ^. pName ]
  tellLn p ("Players online: " ++ listify names ++ ".")

inventory :: SimpleVerb
inventory p = do
  objs <- select (onPlayer p)
  case null objs of
    True  -> tellLn p "You aren't carrying anything."
    False -> tellLn p $ "You are carrying: " ++ listify (map (oNoun_ . snd) objs) ++ "."

get :: Verb
get args pid = do
    mr <- getA (mPlayers .> byId pid .> pRoom)
    case mr of
      Nothing -> tellLn pid "You aren't anywhere."
      Just r  -> parse (doGet <$> pObject (inRoom r)) args pid
  where
    doGet item p = mObjects .> byId item .> oLoc %= OnPlayer p

dropObj :: Verb
dropObj args pid = do
    mr <- (^. pRoom) <$> selectById pid
    case mr of
      Nothing -> tellLn pid "You aren't anywhere."
      Just r  -> parse (doDrop r <$> pObject (onPlayer pid)) args pid
  where
    doDrop room item pid = mObjects .> byId item .> oLoc %= InRoom room

give :: Verb
give args pid = do
    mr <- (^. pRoom) <$> selectById pid
    parse (doGive mr <$> pObject (onPlayer pid) <* pWord "to" <*> pPlayer (pRoom ^.= mr)) args pid
  where
    doGive mr oid tid _ = do
      -- Set new location.
      update oid (oLoc ^= OnPlayer tid)
      -- Fetch names.
      objName         <- (^. oNoun) <$> selectById oid
      Just sourceName <- (^. pName) <$> selectById pid
      Just targetName <- (^. pName) <$> selectById tid
      -- Send messages.
      tellLn pid $ "You give " ++ objName ++ " to " ++ targetName ++ "."
      tellLn tid $ sourceName ++ " gives you " ++ objName ++ "."
      when (isJust mr) $ do
        sayLn (fromJust mr) ((/= pid) & (/= tid)) $ sourceName ++ " gives " ++ objName ++ " to " ++ targetName ++ "."

(&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(f & g) x = f x && g x

(^.=) :: Eq a => Accessor r a -> a -> r -> Bool
(field ^.= val) rec = (rec ^. field) == val


installVerbs :: Mud ()
installVerbs = do
  mkSimpleVerb "l" look
  mkSimpleVerb "look" look
  mkSimpleVerb "n" (move "north")
  mkSimpleVerb "e" (move "east")
  mkSimpleVerb "s" (move "south")
  mkSimpleVerb "w" (move "west")
  mkSimpleVerb "u" (move "up")
  mkSimpleVerb "d" (move "down")
  mkSimpleVerb "q" quit
  mkSimpleVerb "quit" quit
  mkSimpleVerb "who" who
  mkVerb "say" playerSay
  mkVerb "'" playerSay
  mkVerb "chat" chat
  mkVerb "chat:" chatEmote
  mkVerb "emote" emote
  mkVerb ":" emote
  mkSimpleVerb "help" help
  mkSoul "nod" "nod." "nods."
  mkSoul "shake" "shake your head." "shakes their head."
  mkSoul "smile" "smile." "smiles."
  mkSoul "grin" "grin." "grins."
  mkSoul "wave" "wave." "waves."
  mkVerb "tell" playerTell
  mkVerb "t" playerTell
  mkSimpleVerb "i" inventory
  mkSimpleVerb "inv" inventory
  mkSimpleVerb "inventory" inventory
  mkVerb "get" get
  mkVerb "drop" dropObj
  mkVerb "give" give
