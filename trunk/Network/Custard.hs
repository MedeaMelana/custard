{-# OPTIONS -fglasgow-exts #-}

module Network.Custard where

import Network
import System.IO
import Control.Concurrent
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Char as C
import qualified Data.Map as M
import Data.Maybe

port :: Num a => a
port = 5678

main :: IO ()
main = do
  putStrLn $ "Listening on port " ++ show port ++ "..."
  world <- execStateT mkWorld emptyMud
  vWorld <- newMVar world
  listenOn (PortNumber port) >>= loopAccept vWorld

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

notEmpty :: String -> Bool
notEmpty = not . all C.isSpace

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

data Player = Player
  { pName :: String
  , pRoom :: MVar Room
  , pWrite :: String -> IO ()
  , pGetLine :: IO String
  }

instance Eq Player where
  p1 == p2 = pName p1 == pName p2

instance Ord Player where
  compare p1 p2 = compare (pName p1) (pName p2)

pWriteLn :: Player -> String -> IO ()
pWriteLn p s = pWrite p s >> pWrite p "\n"

data Room = Room 
  { rName :: String
  , rPlayers :: MVar (S.Set Player)
  , rExits :: M.Map String Room
  }

type Mud = StateT MudState IO

data MudState = MudState
  { mRooms :: [Room]
  }

emptyMud :: MudState
emptyMud = MudState []

mkRoom :: String -> [(String, Room)] -> Mud Room
mkRoom name exits = do
  vPlayers <- liftIO $ newMVar S.empty
  let room = Room name vPlayers (M.fromList exits)
  modify (\s -> s { mRooms = room : mRooms s })
  return room

mkWorld :: Mud ()
mkWorld = mdo
  nw <- mkRoom "Northwest corner" [("e", ne), ("s", sw)]
  ne <- mkRoom "Northeast corner" [("w", nw), ("s", se)]
  se <- mkRoom "Southeast corner" [("n", ne), ("w", sw)]
  sw <- mkRoom "Southwest corner" [("n", nw), ("e", se)]
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

listify :: [String] -> String
liftify [] = ""
listify [x] = x
listify [x, y] = x ++ " and " ++ y
listify (x:xs) = x ++ ", " ++ listify xs

enter :: Player -> Room -> IO ()
enter p r = do
  putMVar (pRoom p) r
  mapMVar (rPlayers r) (S.insert p)
  return ()

mapMVar :: MVar a -> (a -> a) -> IO (a, a)
mapMVar m f = modifyMVar m t where
  t v = return (v', (v, v')) where 
    v' = f v

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

-- | Asks the player a question repeatedly, until the predicate is satisfied.
query :: Player -> (String -> Bool)  -> String -> IO String
query p ok prompt = loop where
  loop = do
    pWrite p prompt
    line <- pGetLine p
    if ok line
      then return (trim line)
      else loop

trim :: String -> String
trim = f . f where f = reverse . dropWhile C.isSpace

quit :: Player -> IO ()
quit p = do
  Just room <- exit p
  roomSay room (const True) $ pName p ++ " suddenly disappears in a bright flash!"
  pWriteLn p "Thank you for playing!"

emote :: Player -> String -> IO ()
emote p msg = do
  room <- readMVar (pRoom p)
  roomSay room (const True) $ pName p ++ " " ++ msg
