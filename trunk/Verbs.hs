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
playerTell = parse (doTell <$> pPlayer <*> pRest)
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
