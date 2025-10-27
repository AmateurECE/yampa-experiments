{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}

module ActivationSwitch.Switch
  ( parseKeyEventsSF,
    readKeyEvents,
    setEnabledSF,
    enabledKeysSF,
    keySF,
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

--
-- Signal Functions
--

-- Derive the state of a switch s for all times t by filtering events on the
-- switch s and holding the previous state.
keySF :: Char -> SF (Event KeyState) KeyState
keySF target = proc allKeys -> do
  key <- filterByKey -< allKeys
  stable <- hold (KeyState target Unpressed) -< key
  returnA -< stable
  where
    filterByKey = arr (filterE (\e -> keyId e == target))

-- Shunt the state of disabled switches to "Unpressed", effectively ignoring
-- all changes in state that occur on these keys.
enabledKeysSF ::
  forall n.
  (KnownNat n) =>
  SF (V.Vector n Bool, V.Vector n KeyState) (V.Vector n KeyState)
enabledKeysSF = arr $ uncurry $ liftA2 shunt'
  where
    shunt' :: Bool -> KeyState -> KeyState
    shunt' True s = s
    shunt' False s = s {state = Unpressed}

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

-- Set the enabled/disabled state of switches in response to commands.
setEnabledSF ::
  forall n.
  (KnownNat n) =>
  SF (Event (Command n)) (V.Vector n Bool)
setEnabledSF = setParameterSF (set change) (pure True)
