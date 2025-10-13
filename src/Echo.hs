module Echo (echo) where

import Data.IORef
import Data.Time.Clock
import FRP.Yampa hiding (now)
import System.IO

initialize :: IO (Event Char)
initialize = pure NoEvent

sense :: IORef UTCTime -> Bool -> IO (DTime, Maybe (Event Char))
sense lastTimeRef canBlock = do
  hasInput <- hReady stdin
  mChar <-
    if hasInput || canBlock
      then (Just . Event) <$> getChar
      else pure $ Just NoEvent

  now <- getCurrentTime
  lastTime <- readIORef lastTimeRef
  writeIORef lastTimeRef now
  let dt = realToFrac (now `diffUTCTime` lastTime)
  return (dt, mChar)

actuate :: Bool -> Event Char -> IO Bool
actuate hasChanged value = do
  case (hasChanged, value) of
    (True, Event c) -> hPutChar stdout c
    (_, _) -> pure ()
  return False

application :: SF (Event Char) (Event Char)
application = identity

echo :: IO ()
echo = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  lastTimeRef <- getCurrentTime >>= newIORef
  reactimate initialize (sense lastTimeRef) actuate application
