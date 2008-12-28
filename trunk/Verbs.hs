module Verbs where

import Mud
import MudTypes
import Text
import Data.Accessor

playerSay :: Verb
playerSay msg p = do
  let trimmed = trim msg
  Just room <- getA (mPlayers .> byId p .> pRoom)
  Just name <- getA (mPlayers .> byId p .> pName)
  sayLn room (/= p) (name ++ " says: " ++ trimmed)
  tellLn p ("You say: " ++ trimmed)

chat :: Verb
chat msg p = do
  Just name <- getA (mPlayers .> byId p .> pName)
  let trimmed = trim msg
  shoutLn (const True) ("(chat) " ++ name ++ ": " ++ trimmed)

emote :: Verb
emote msg p = do
  Just room <- getA (mPlayers .> byId p .> pRoom)
  Just name <- getA (mPlayers .> byId p .> pName)
  let line = name ++ " " ++ trim msg
  sayLn room (/= p) line
  tellLn p ("You emote: " ++ line)

installVerbs :: Mud ()
installVerbs = do
  mkLoneVerb "l" look
  mkLoneVerb "look" look
  mkLoneVerb "n" (move "north")
  mkLoneVerb "e" (move "east")
  mkLoneVerb "s" (move "south")
  mkLoneVerb "w" (move "west")
  mkLoneVerb "u" (move "up")
  mkLoneVerb "d" (move "down")
  mkVerb "say" playerSay
  mkVerb "'" playerSay
  mkVerb "chat" chat
  mkVerb "emote" emote
  mkVerb ":" emote
