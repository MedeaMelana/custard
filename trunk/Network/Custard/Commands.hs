module Network.Custard.Commands where

import Network.Custard.Core
import Network.Custard.Util
import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Concurrent
import Control.Monad (when)

quit :: Player -> IO ()
quit p = do
  Just room <- exit p
  roomSay room (const True) $ pName p ++ " suddenly disappears in a bright flash!"
  pWriteLn p "Thank you for playing!"

emote :: Player -> String -> IO ()
emote p msg = do
  room <- readMVar (pRoom p)
  roomSay room (const True) $ pName p ++ " " ++ msg

roomSay :: Room -> (Player -> Bool) -> String -> IO ()
roomSay room ok msg = do
  ps <- fmap (filter ok . S.toList) $ readMVar (rPlayers room)
  let say p = pWriteLn p msg
  sequence (map say ps)
  return ()

say :: Player -> String -> IO ()
say p msg = do
  room <- readMVar (pRoom p)
  pWriteLn p $ "You say: " ++ msg
  roomSay room (/= p) (pName p ++ " says: " ++ msg)

exit :: Player -> IO (Maybe Room)
exit p = do
  mRoom <- tryTakeMVar (pRoom p)

  when (isJust mRoom) $ do
    mapMVar (rPlayers (fromJust mRoom)) (S.delete p)
    return ()

  return mRoom

move :: Player -> Room -> IO ()
move p r = do
  mr <- exit p
  when (isJust mr) $ do
    roomSay (fromJust mr) (const True) (pName p ++ " leaves the room.")
  enter p r
  roomSay r (/= p) (pName p ++ " enters the room.")
  look p

enter :: Player -> Room -> IO ()
enter p r = do
  putMVar (pRoom p) r
  mapMVar (rPlayers r) (S.insert p)
  return ()


look :: Player -> IO ()
look p = do
  room <- readMVar (pRoom p)
  players <- fmap (filter (/= p) . S.toList) $ readMVar (rPlayers room)
  let playerLines = case players of
                      []  -> []
                      [p'] -> [pName p' ++ " is here."]
                      _   -> [(listify . map pName) players ++ " are here."]
  let exits = concat . L.intersperse ", " . M.keys . rExits $ room
  let output = unlines $ [ rName room ] ++ playerLines ++ [ "Exits: " ++ exits ]
  pWrite p output

