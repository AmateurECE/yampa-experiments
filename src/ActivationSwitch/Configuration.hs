{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeApplications #-}

module ActivationSwitch.Configuration
  ( Command (..),
    Direction (..),
    configurationSF,
  )
where

import ActivationSwitch.Therapy
import FRP.Yampa hiding (left, right)
import Prelude hiding (sequence)

data Direction = Up | Down

data Command = Command
  { keyIndex :: Int,
    direction :: Direction
  }

data Controller = Controller
  { selectedKey :: Int,
    numberOfKeys :: Int,
    command :: Event Command
  }

maxKey :: Controller -> Int
maxKey c = (numberOfKeys c) - 1

left :: Controller -> Controller
left c = case selectedKey c of
  0 -> c
  n -> c {selectedKey = n - 1}

right :: Controller -> Controller
right c = case compare (selectedKey c) (maxKey c) of
  GT -> c {selectedKey = maxKey c}
  LT -> c {selectedKey = (selectedKey c) + 1}
  EQ -> c

up :: Controller -> Controller
up c = c {command = Event $ Command (selectedKey c) Up}

down :: Controller -> Controller
down c = c {command = Event $ Command (selectedKey c) Down}

step :: Controller -> Event KeyState -> Controller
step controller (Event e) = case (keyId e, state e) of
  ('h', Pressed) -> left controller
  ('l', Pressed) -> right controller
  ('j', Pressed) -> down controller
  ('k', Pressed) -> up controller
  _ -> controller
step controller NoEvent = controller

configurationSF :: Int -> SF (Event KeyState) (Int, Event Command)
configurationSF keys = proc event' -> do
  rec let controller = step (Controller previous keys NoEvent) event'
      previous <- iPre 0 -< selectedKey controller
      let output = (command controller)
      let current = selectedKey controller
  returnA -< (current, output)
