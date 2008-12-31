module Verbs where

import Mud
import MudTypes
import Text
import Data.Accessor
import qualified Data.Map as M
import qualified Data.Char as C

playerSay :: Verb
playerSay msg p = do
  let trimmed = trim msg
  if null trimmed
    then tellLn p "Say what?"
    else do
      Just room <- getA (mPlayers .> byId p .> pRoom)
      Just name <- getA (mPlayers .> byId p .> pName)
      sayLn room (/= p) (name ++ " says: " ++ trimmed)
      tellLn p ("You say: " ++ trimmed)

chat :: Verb
chat msg = chatEmote (": " ++ msg)

chatEmote :: Verb
chatEmote msg p = do
  case trim msg of
    []    -> tellLn p "Chat what?"
    c:cs  -> do
      let sep = if C.isPunctuation c then "" else " "
      Just name <- getA (mPlayers .> byId p .> pName)
      shoutLn (const True) ("(chat) " ++ name ++ sep ++ c:cs)

emote :: Verb
emote msg p = do
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

    

help :: Verb
help _ p = do
  verbs <- M.keys `fmap` getA mVerbs
  tellLn p ("Available commands: " ++ listify verbs ++ ".")

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
  mkVerb "say" playerSay
  mkVerb "'" playerSay
  mkVerb "chat" chat
  mkVerb "chat:" chatEmote
  mkVerb "emote" emote
  mkVerb ":" emote
  mkVerb "help" help
  mkSoul "nod" "nod." "nods."
  mkSoul "shake" "shake your head." "shakes their head."
  mkSoul "smile" "smile." "smiles."
  mkSoul "grin" "grin." "grins."
  mkSoul "wave" "wave." "waves."
