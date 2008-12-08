{-# OPTIONS -fglasgow-exts #-}

module Network.Custard.Engine where

import Control.Monad.State
import Control.Concurrent
import Network
import Network.Custard.Core
import System.IO
import Network.Custard.Util
import Network.Custard.Commands
import qualified Data.Map as M

runCustard :: Int -> Mud () -> IO ()
runCustard port world = do
  putStrLn $ "Listening on port " ++ show port ++ "..."
  vWorld <- newMVar emptyMud
  runMud world vWorld
  listenOn (PortNumber $ fromIntegral port) >>= loopAccept vWorld

loopAccept :: MVar MudState -> Socket -> IO ()
loopAccept vWorld sock = do
  hSetBuffering stdout NoBuffering
  (h, hostname, _) <- accept sock
  forkIO (handleClient vWorld h hostname)
  loopAccept vWorld sock

handleClient :: MVar MudState -> Handle -> HostName -> IO ()
handleClient vWorld h hostname = mdo
  putStrLn $ "Incoming connection from " ++ hostname ++ "."

  vRoom <- newEmptyMVar
  let player = Player name vRoom (\s -> hPutStr h s >> hFlush h) (hGetLine h)

  pWriteLn player "Welcome!"
  name <- query player (\s -> notEmpty s && length (trim s) < 12) "By what name do you wish to be known? "

  putStrLn $ "Welcome, " ++ name ++ "."

  room <- runMud (gets $ head . mRooms) vWorld
  enter player room
  look player
  roomSay room (/= player) (name ++ " suddenly appears beside you.")

  loopReadCommand player
  putStrLn $ "Goodbye, " ++ name ++ "."
  hClose h

loopReadCommand :: Player -> IO ()
loopReadCommand p = loop where
  loop = do
    cmd <- query p notEmpty "> "
    room <- liftIO $ readMVar (pRoom p)
    case cmd of
      "l"                 -> look p >> loop
      'l':'o':'o':'k':_   -> look p >> loop
      "q"                 -> quit p
      'q':'u':'i':'t':_   -> quit p
      's':'a':'y':' ':msg -> say p msg >> loop
      '\'':msg            -> say p msg >> loop
      ':':msg         -> emote p msg >> loop
      _                   -> case M.lookup cmd (rExits room) of
        Nothing -> pWriteLn p ("Unrecognised command: " ++ cmd) >> loop
        Just room' -> move p room' >> loop

runMud :: Mud a -> MVar MudState -> IO a
runMud act vWorld = do
  world <- takeMVar vWorld
  (res, world') <- runStateT act world
  putMVar vWorld world'
  return res
