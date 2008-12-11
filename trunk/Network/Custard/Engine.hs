{-# OPTIONS -fglasgow-exts #-}

module Network.Custard.Engine where

import Control.Monad.State
import Control.Concurrent
import Network
import Network.Custard.Core
import System.IO
import Network.Custard.Util
-- import Network.Custard.Commands
import qualified Data.Map as M

runCustard :: Int -> IO ()
runCustard port = runMud $ do
  liftIO $ putStrLn $ "Listening on port " ++ show port ++ "..."
  -- what are we running here?
  -- vWorld <- newMVar emptyMud
  -- runMud world vWorld
  -- world
  -- serverToClient <- newChan
  -- clientToServer <- newChan
  state <- get
  let channel = mChan state
  liftIO $ forkIO $ handleServer channel
  liftIO $ listenOn (PortNumber $ fromIntegral port) >>= loopAccept

handleServer channel = do
  msg <- liftIO readChan
  let cont = handleServer channel
  case msg of
    NewPlayer name chan -> newPlayer name chan
    Died      name -> undefined
    Input name str -> undefined

newPlayer name chan = do
  room <- gets $ head . mRooms
  enter player room
  look player
  roomSay room (/= player) (name ++ " suddenly appears beside you.")
  liftIO $ writeChan chan ServerAck

handleClient = undefined

loopAccept :: Socket -> IO ()
loopAccept sock = do
  hSetBuffering stdout NoBuffering
  (h, hostname, _) <- accept sock
  forkIO (handleClient h hostname)
  loopAccept sock

-- TODO mayor fixing needed ahead

{-
handleClient :: MVar MudState -> Handle -> HostName -> IO ()
handleClient vWorld h hostname = mdo
  putStrLn $ "Incoming connection from " ++ hostname ++ "."

  vRoom <- newEmptyMVar
  let exitFirst err = rudeQuit player >> ioError err
  let safePut s = catch (hPutStr h s >> hFlush h) exitFirst
  let safeGet   = catch (hGetLine h)              exitFirst
  let player = Player name vRoom safePut safeGet

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

-- runMud :: Mud a -> MudState -> a
-- runMud act = fst . runStateT act
-}
