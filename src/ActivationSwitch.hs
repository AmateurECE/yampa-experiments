module ActivationSwitch (activationSwitch) where

import ActivationSwitch.App
import ActivationSwitch.Switch (readKeyEvents)
import Control.Monad
import qualified Data.ByteString as BS
import Data.IORef
import Data.Time
import FRP.Yampa hiding (now)
import Foreign
import System.IO
import System.Posix

sense :: IO (Event BS.ByteString) -> IORef UTCTime -> Bool -> IO (DTime, Maybe (Event BS.ByteString))
sense readAction lastTimeRef _ = do
  events <- readAction
  now <- getCurrentTime
  lastTime <- readIORef lastTimeRef
  writeIORef lastTimeRef now
  let dt = realToFrac (now `diffUTCTime` lastTime)
  return (dt, Just events)

actuate :: IORef Bool -> IORef (Maybe (UIState n)) -> Bool -> (UIState n) -> IO Bool
actuate terminate previousRef hasChanged value = do
  previous <- readIORef previousRef
  when (hasChanged && (maybe True (/= value) previous)) $ do
    writeIORef previousRef $ Just value
    renderUI value
    hFlush stdout
  return =<< readIORef terminate

activationSwitch :: IO ()
activationSwitch = do
  -- Terminate gracefully when Ctrl-C is pressed
  terminate <- newIORef False
  let exit = writeIORef terminate True
  _ <- installHandler sigINT (Catch exit) Nothing

  -- Disable buffering and echo on stdin. We need to read each key event when
  -- it happens, and we use the terminal to render a TUI.
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  -- Enable the kitty keyboard protocol, which allows us to receive key release
  -- events.
  putStr "\x1B[>2u"
  hFlush stdout

  let size = 32
  buf <- mallocBytes size
  let readAction = readKeyEvents buf size

  lastTimeRef <- getCurrentTime >>= newIORef
  uiState <- newIORef Nothing

  reactimate
    initialize
    (sense readAction lastTimeRef)
    (actuate terminate uiState)
    application

  -- Restore the terminal mode
  putStrLn "\x1B[<2u"
  hFlush stdout
  free buf
