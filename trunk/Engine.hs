{-# OPTIONS -fglasgow-exts #-}

module Engine where

import Prelude hiding (catch)
import Network
import System.IO
import Control.Concurrent
import Control.Monad.State
import Control.Exception
import Control.Arrow ((***))
import Data.IORef
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

type ServerState = IORef (MudState, PlayerMap)

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
  let handle :: SomeException -> IO ()
      handle _ = tellServer (Disconnected p)
  flip catch handle $ do
    hGetLine h >>= tellServer . Input p . sanitizeInput
    threadDelay 100000 -- hinder spam
    loop

-- | One per client: listens for ClientMessages from the server.
runServerListener :: Id Player -> Handle -> Input ClientMessage -> IO ()
runServerListener p h listenToServer = do
  let loop = runServerListener p h listenToServer
  msg <- listenToServer
  case msg of
    Output s      -> do
      let handle :: SomeException -> IO ()
          handle _ = return ()
      catch (hPutStr h s >> hFlush h) handle >> loop
    Kill          -> hClose h
    Registered _  -> error "unexpected message from server: Registered"


type PlayerMap = M.Map (Id Player) (Output ClientMessage)

-- | The server processes messages from clients.
runServer :: Input ServerMessage -> Mud () -> IO ()
runServer readMessage mkWorld = do
  vState <- newIORef (emptyMud, M.empty)
  runMud vState mkWorld
  let loop = do
        msg <- readMessage
        case msg of
          NewClient tellClient -> do
            Right p <- runMud' vState newPlayer  -- note: doesn't flush effects yet
            updatePlayerMap vState (M.insert p tellClient)
            tellClient (Registered p)
            runMud vState (prompt p)  -- note: does flush effects, which is okay now map contains player
            loop
          Input p line -> do
            result <- runMud vState (execute p line >> prompt p)
            case result of
              Left e -> do
                putStrLn ("Error: " ++ show e)
                putStrLn ("On executing input: " ++ show line)
              Right _ -> return ()
            loop
          Disconnected p -> do
            -- either the client disconnected
            --    or we closed the connection
            (_, ps) <- readIORef vState
            when (p `M.member` ps) $ do
              runMud vState (disconnected p)
              return ()
            loop
          Shutdown -> undefined
  loop

updatePlayerMap :: ServerState -> (PlayerMap -> PlayerMap) -> IO ()
updatePlayerMap vState f = modifyIORef vState (id *** f)

deepTupleSeq :: (a, b) -> (a, b)
deepTupleSeq (x, y) = x `seq` y `seq` (x, y)

-- | Runs a Mud computation, updating the state and returning the computation's result.
--   Doesn't execute any effects the action might have had.
runMud' :: ServerState -> Mud a -> IO (Either SomeException a)
runMud' vState action = do
  (mud, players) <- readIORef vState
  result <- try $ evaluate $ deepTupleSeq $ runState action mud
  case result of
    Left e -> return (Left e)
    Right (v, mud') -> do
      writeIORef vState (mud', players)
      return (Right v)

-- | Runs a Mud computation, updating the state and returning the computation's result.
--   Then executes any effects the action might have had.
runMud :: ServerState -> Mud a -> IO (Either SomeException a)
runMud vState action = do
  result <- runMud' vState action
  case result of
    Left e  -> return (Left e)
    Right v -> do
      executeEffects vState
      return (Right v)

-- | Executes the effects accumulated in the state.
executeEffects :: ServerState -> IO ()
executeEffects vState = do
  Right effs <- runMud' vState flushEffects
  (_, ps) <- readIORef vState
  forM_ effs $ \eff -> case eff of
    Message p m -> do
      let tellClient = ps M.! p
      tellClient (Output m)
    Logoff p -> do
      runMud' vState (doQuit p)
      let tellClient = ps M.! p
      tellClient Kill
      updatePlayerMap vState (M.delete p)
