module Verbs where

import Mud
import MudTypes
import Text
import Data.Accessor
import qualified Data.Map as M

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
  mkVerb "say" playerSay
  mkVerb "'" playerSay
  mkVerb "chat" chat
  mkVerb "emote" emote
  mkVerb ":" emote
  mkVerb "help" help
