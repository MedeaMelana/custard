{-# OPTIONS -fglasgow-exts #-}

module Engine where

import Network
import System.IO
import Control.Concurrent
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Map as M
import MudTypes
import Mud
import Text

type Input  a =      IO a
type Output a = a -> IO ()

-- Messages to the server.
data ServerMessage
  = NewClient (Output ClientMessage)
  | Input (Id Player) String
  | Disconnected (Id Player)
  | Shutdown

-- Messages to the client.
data ClientMessage
  = Registered (Id Player)
  | Output String
  | Kill

-- | Spawns the server, then becomes the listener.
runCustard :: PortNumber -> Mud () -> IO ()
runCustard port mkWorld = do
  putStrLn $ "Listening on port " ++ show port ++ "..."
  serverChan <- newChan  -- messages to the server
  forkIO $ runServer (readChan serverChan) mkWorld
  listenOn (PortNumber port) >>= runListener (writeChan serverChan)

-- | Listens for incoming connections.
runListener :: Output ServerMessage -> Socket -> IO ()
runListener tellServer sock = do
  let loop = runListener tellServer sock
  -- Wait for connection.
  (h, host, _) <- accept sock
  putStrLn ("Incoming connection from " ++ host)  
  -- Create new channel and obtain player id.
  clientChan <- newChan
  tellServer (NewClient (writeChan clientChan))
  (Registered p) <- readChan clientChan
  -- Fork client threads.
  forkIO $ runInputListener p h tellServer
  forkIO $ runServerListener p h (readChan clientChan)
  -- Start listening again.
  loop

-- | One per client: listens for messages from the client connection.
runInputListener :: Id Player -> Handle -> Output ServerMessage -> IO ()
runInputListener p h tellServer = do
  let loop = runInputListener p h tellServer
  line <- hGetLine h
  tellServer (Input p $ sanitizeInput line)
  -- todo: catch exceptions, test for eof
  loop

-- | One per client: listens for ClientMessages from the server.
runServerListener :: Id Player -> Handle -> Input ClientMessage -> IO ()
runServerListener p h listenToServer = do
  let loop = runServerListener p h listenToServer
  msg <- listenToServer
  case msg of
    Output s      -> hPutStr h s >> hFlush h >> loop
    Kill          -> hClose h
    Registered _  -> error "unexpected message from server: Registered"


type PlayerMap = M.Map (Id Player) (Output ClientMessage)

-- | The server processes messages from clients.
runServer :: Input ServerMessage -> Mud () -> IO ()
runServer readMessage mkWorld = loop M.empty world where
  world = execState mkWorld emptyMud
  loop players state = do
    msg <- readMessage
    case msg of
      NewClient tellClient -> do
        let (p, state') = runState newPlayer state
        let players' = M.insert p tellClient players
        tellClient (Registered p)
        (_, state'') <- runMud players' state' (prompt p)  -- cause messages to be sent
        loop players' state''
      Input p line -> do
        (_, state') <- runMud players state (execute p line >> prompt p)
        loop players state'
      Disconnected _ -> undefined
      Shutdown -> undefined

-- | Runs a Mud computation, yielding the result and the new state.
runMud :: PlayerMap -> MudState -> Mud a -> IO (a, MudState)
runMud ps state action = do
  let (v, state')   = runState action state
  let (ms, state'') = runState flushMessages state'
  sendMessages ps ms
  return (v, state'')

-- | Sends all messages to the right clients.
sendMessages :: PlayerMap -> [Message] -> IO ()
sendMessages ps = mapM_ $ \(p, m) -> do
  let tellClient = ps M.! p
  tellClient (Output m)
