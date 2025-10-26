{-# LANGUAGE Arrows #-}

module ActivationSwitch.Switch
  ( parseKeyEventsSF,
    readKeyEvents,
    setEnabledSF,
  )
where

import ActivationSwitch.Configuration
import ActivationSwitch.Therapy
import Control.Lens hiding (set)
import qualified Data.ByteString as BS
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import FRP.Yampa hiding (after, count, event)
import Foreign
import Linear
import System.IO
import Text.Regex.TDFA hiding (after)

toString :: BS.ByteString -> String
toString = T.unpack . E.decodeUtf8

-- Parse input from terminal emulator as a KeyEvent
parseKeyEvents :: String -> [KeyState]
parseKeyEvents bytes = case bytes of
  ('\ESC' : '[' : rest) -> parseUnpressed rest
  (key : rest) -> KeyState key Pressed : parseKeyEvents rest
  [] -> []
  where
    parseUnpressed event =
      let (_, _, after, groups) = event =~ "([0-9]+);[0-9]:3u" :: (String, String, String, [String])
       in case groups of
            [key] -> KeyState (chr $ read key) Unpressed : parseKeyEvents after
            _ -> parseKeyEvents event

-- Read key events from stdin
readKeyEvents :: Ptr Word8 -> Int -> IO (Event BS.ByteString)
readKeyEvents buffer size = do
  count <- hGetBufNonBlocking stdin buffer size
  if count > 0
    then do
      bytes <- peekArray count buffer
      return $ Event $ BS.pack bytes
    else return NoEvent

change :: Direction -> Bool -> Bool
change Up False = True
change Down False = False
change Up True = True
change Down True = False

defaults :: V3 Bool
defaults = pure True

set :: V3 Bool -> [Event Command] -> V3 Bool
set = foldl step
  where
    step v (Event e) = applyCommand e v
    step v NoEvent = v

    applyCommand :: Command -> V3 Bool -> V3 Bool
    applyCommand c =
      let update = change $ direction c
       in case keyIndex c of
            0 -> (& _x %~ update)
            1 -> (& _y %~ update)
            2 -> (& _z %~ update)
            _ -> id

--
-- Signal Functions
--

fanOutEvents :: Event [a] -> [Event a]
fanOutEvents (Event items) = map Event items
fanOutEvents NoEvent = []

-- A signal function that receives key events as binary data and emits a stream
-- of KeyState events.
parseKeyEventsSF :: SF (Event BS.ByteString) ([Event KeyState])
parseKeyEventsSF = arr $ fanOutEvents . fmap toEvents
  where
    toEvents = parseKeyEvents . toString

-- TODO: I should probably refactor all of these to take a instead of [a]
-- I think I could also pull a polymorphic function out of here?
setEnabledSF :: SF ([Event Command]) (V3 Bool)
setEnabledSF = proc commands -> do
  rec let current = set previous commands
      previous <- iPre $ defaults -< current
  returnA -< current
