{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time
import Data.Word
import FRP.Yampa hiding (after, count, event, now)
import Foreign
import System.IO
import System.Posix
import Text.Regex.TDFA hiding (after, match)

data SwitchState = Unpressed | Pressed deriving (Show, Eq)

data KeyState = KeyState
  { keyId :: Char,
    state :: SwitchState
  }
  deriving (Show)

data TherapyState = Active Char | Inactive | Blocked

instance Show TherapyState where
  show (Active _) = "Active"
  show _ = "Inactive"

pressedSwitches :: [KeyState] -> [KeyState]
pressedSwitches = filter (\s -> state s == Pressed)

therapy :: TherapyState -> [KeyState] -> TherapyState
therapy Inactive states = case pressedSwitches states of
  [one] -> Active $ keyId one
  _ -> Inactive
therapy (Active a) states =
  -- INVARIANT: keyIds are constant. This makes the application sound, but also
  -- ensures head does not throw in the following expression.
  let currentState = state $ head $ filter (\s -> keyId s == a) states
   in case currentState of
        Pressed -> Active a
        Unpressed -> Blocked
therapy Blocked states = case pressedSwitches states of
  [] -> Inactive
  _ -> Blocked

toString :: BS.ByteString -> String
toString = T.unpack . E.decodeUtf8

-- Parse input from terminal emulator as a KeyEvent
parseKeyEvents :: String -> [KeyState]
parseKeyEvents bytes = case bytes of
  ('\x1B' : '[' : rest) -> parseUnpressed rest
  (key : rest) -> KeyState key Pressed : parseKeyEvents rest
  [] -> []
  where
    parseUnpressed event =
      let (_, _, after, groups) = event =~ "([0-9]+);[0-9]:3u" :: (String, String, String, [String])
       in case groups of
            [key] -> KeyState (chr $ read key) Unpressed : parseKeyEvents after
            _ -> parseKeyEvents event

initialize :: IO (Event a)
initialize = pure NoEvent

sense :: Ptr Word8 -> Int -> IORef UTCTime -> Bool -> IO (DTime, Maybe (Event BS.ByteString))
sense buffer size lastTimeRef _ = do
  count <- hGetBufNonBlocking stdin buffer size

  now <- getCurrentTime
  lastTime <- readIORef lastTimeRef
  writeIORef lastTimeRef now
  let dt = realToFrac (now `diffUTCTime` lastTime)

  if count > 0
    then do
      bytes <- peekArray count buffer
      return (dt, Just $ Event $ BS.pack bytes)
    else return (dt, Just NoEvent)

actuate :: IORef Bool -> Bool -> TherapyState -> IO Bool
actuate terminate hasChanged value = do
  when hasChanged $ do
    putStr $ "\rTherapy: " ++ show value ++ "\ESC[0K"
    hFlush stdout
  readIORef terminate >>= return

--
-- Signal Functions
--

fanOutEvents :: Event [a] -> [Event a]
fanOutEvents (Event items) = map Event items
fanOutEvents NoEvent = []

parseKeyEventsSF :: SF (Event BS.ByteString) ([Event KeyState])
parseKeyEventsSF = arr $ fanOutEvents . fmap toEvents
  where
    toEvents = parseKeyEvents . toString

keySF :: Char -> SF ([Event KeyState]) KeyState
keySF target = hold (KeyState target Unpressed) <<< (arr $ mergeEvents) <<< filterByKey
  where
    filterByKey = arr (fmap $ filterE (\e -> keyId e == target)) :: SF ([Event KeyState]) ([Event KeyState])

controller :: SF (Event BS.ByteString) [KeyState]
controller = arr (\(a, (b, c)) -> [a, b, c]) <<< ((keySF '1') &&& (keySF '2') &&& (keySF '3')) <<< parseKeyEventsSF

therapySF :: SF [KeyState] TherapyState
therapySF = loopPre Inactive $ arr $ \(keyStates, previousState) ->
  let currentState = therapy previousState keyStates
   in (currentState, currentState)

application :: SF (Event BS.ByteString) TherapyState
application = therapySF <<< controller

main :: IO ()
main = do
  -- Terminate gracefully when Ctrl-C is pressed
  terminate <- newIORef False
  let exit = writeIORef terminate True
  _ <- installHandler sigINT (Catch exit) Nothing

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  -- Enable the kitty keyboard protocol
  putStr "\x1B[>2u"
  hFlush stdout

  let size = 32
  buf <- mallocBytes size

  lastTimeRef <- getCurrentTime >>= newIORef
  reactimate initialize (sense buf size lastTimeRef) (actuate terminate) application

  -- Restore the terminal mode
  putStrLn "\x1B[<2u"
  hFlush stdout
  free buf
