{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Word
import Foreign.Marshal
import Numeric
import System.Exit (exitSuccess)
import System.IO
import System.Posix
import Text.Regex.TDFA hiding (after, match)

data SwitchState = Unpressed | Pressed deriving (Show)

data KeyEvent = KeyEvent
  { id :: Char,
    state :: SwitchState
  }
  deriving (Show)

-- NOTE: Happy paths first
parseInput :: [Char] -> [KeyEvent]
parseInput bytes = case bytes of
  ('\x1B' : '[' : rest) -> parseKeyEvent rest
  (key : rest) -> KeyEvent key Pressed : parseInput rest
  [] -> []
  where
    parseKeyEvent event =
      let (_, _, after, groups) = event =~ "([0-9]+);[0-9]:3u" :: (String, String, String, [String])
       in case groups of
            [key] -> KeyEvent (chr $ read key) Unpressed : parseInput after
            _ -> parseInput event

main :: IO ()
main = do
  terminate <- newIORef False
  _ <- installHandler sigINT (Catch $ exit terminate) Nothing

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  -- Enable the kitty keyboard protocol
  putStr "\x1B[>2u"
  hFlush stdout

  let size = 32
  buf <- mallocBytes size

  let cleanup = do
        -- Restore the terminal mode
        putStr "\x1B[<2u"
        hFlush stdout
        free buf
        putStrLn "Exiting gracefully..."
        exitSuccess

  forever $ do
    threadDelay 100000 -- 0.1s
    count <- hGetBufNonBlocking stdin buf size
    when (count > 0) $ do
      bytes <- peekArray count buf :: IO [Word8]
      putStrLn $ show $ parseInput $ T.unpack $ E.decodeUtf8 $ BS.pack bytes
    shouldTerminate <- readIORef terminate
    when shouldTerminate cleanup

exit :: IORef Bool -> IO ()
exit signal = writeIORef signal True
