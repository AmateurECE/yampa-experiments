{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}

module ActivationSwitch.Switch
  ( parseKeyEventsSF,
    readKeyEvents,
    setEnabledSF,
  )
where

import ActivationSwitch.Configuration
import ActivationSwitch.Therapy
import qualified Data.ByteString as BS
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Vector.Sized as V
import FRP.Yampa hiding (after, count, event)
import Foreign
import GHC.TypeLits
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

defaults :: forall n. (KnownNat n) => V.Vector n Bool
defaults = pure True

set :: V.Vector n Bool -> Event (Command n) -> V.Vector n Bool
set v NoEvent = v
set v (Event e) =
  let index' = keyIndex e
      value' = change (direction e) $ v `V.index` index'
   in v V.// [(index', value')]

--
-- Signal Functions
--

-- A signal function that receives key events as binary data and emits a stream
-- of KeyState events. Since one binary message may contain multiple key
-- events, we buffer and sequence the events on the output.
parseKeyEventsSF :: SF (Event BS.ByteString) (Event KeyState)
parseKeyEventsSF = proc binary -> do
  rec let current = previous ++ events' binary
      let (head', tail') = uncons' current
      previous <- iPre [] -< tail'
  returnA -< head'
  where
    events' :: (Event BS.ByteString) -> [KeyState]
    events' e = case e of
      (Event bs) -> parseKeyEvents $ toString bs
      NoEvent -> []

    uncons' :: [KeyState] -> (Event KeyState, [KeyState])
    uncons' (x : xs) = (Event x, xs)
    uncons' [] = (NoEvent, [])

setEnabledSF :: forall n. (KnownNat n) => SF (Event (Command n)) (V.Vector n Bool)
setEnabledSF = proc command -> do
  rec let current = set previous command
      previous <- iPre $ defaults -< current
  returnA -< current
