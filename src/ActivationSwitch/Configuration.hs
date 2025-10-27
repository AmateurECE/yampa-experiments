{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ActivationSwitch.Configuration
  ( Command (..),
    Direction (..),
    configurationSF,
    commandSwitchSF,
    setParameterSF,
    set,
  )
where

import ActivationSwitch.Therapy
import qualified ActivationSwitch.Therapy as T
import Data.Finite
import Data.Proxy
import qualified Data.Vector.Sized as V
import FRP.Yampa hiding (left, right)
import GHC.TypeLits
import Prelude hiding (sequence)

-- Direction for a command.
data Direction = Up | Down

-- A command from the user to change configuration.
data Command n = Command
  { keyIndex :: Finite n,
    direction :: Direction
  }

-- Combinator for changing a parameter in a vector in response to a command.
set ::
  (Direction -> a -> a) ->
  V.Vector n a ->
  Command n ->
  V.Vector n a
set change v e =
  let index' = keyIndex e
      value' = change (direction e) $ v `V.index` index'
   in v V.// [(index', value')]

--
-- Controller
--

-- Controller is an abstraction that maps a stream of key events into a stream
-- of commands.
data Controller n = Controller
  { selectedKey :: Finite n,
    command :: Event (Command n)
  }

left :: forall n. (KnownNat n) => Controller n -> Controller n
left c = case getFinite $ selectedKey c of
  0 -> c
  n -> c {selectedKey = finite $ n - 1}

right :: forall n. (KnownNat n) => Controller n -> Controller n
right c =
  let selected' = getFinite $ selectedKey c
      max' = (natVal $ Proxy @n) - 1
   in case compare selected' max' of
        LT -> c {selectedKey = (selectedKey c) + 1}
        _ -> c

up :: Controller n -> Controller n
up c = c {command = Event $ Command (selectedKey c) Up}

down :: Controller n -> Controller n
down c = c {command = Event $ Command (selectedKey c) Down}

-- Step the controller in response to a command.
step ::
  forall n.
  (KnownNat n) =>
  Controller n ->
  Event KeyState ->
  Controller n
step controller (Event e) = case (keyId e, state e) of
  ('h', Pressed) -> left controller
  ('l', Pressed) -> right controller
  ('j', Pressed) -> down controller
  ('k', Pressed) -> up controller
  _ -> controller
step controller NoEvent = controller

--
-- Signal Functions
--

-- Combinator for constructing a SF that sets parameters based on received
-- commands.
setParameterSF ::
  forall n a.
  (a -> Command n -> a) ->
  a ->
  SF (Event (Command n)) a
setParameterSF set' defaults' = proc command' -> do
  rec let current' = update' previous' command'
      previous' <- iPre $ defaults' -< current'
  returnA -< current'
  where
    update' v (Event c) = set' v c
    update' v NoEvent = v

-- Map the stream of key events into a stream of commands by actuating a
-- Controller.
configurationSF ::
  forall n.
  (KnownNat n) =>
  SF (Event KeyState) (Finite n, Event (Command n))
configurationSF = proc event' -> do
  rec let controller = step (Controller previous NoEvent) event'
      previous <- iPre 0 -< selectedKey controller
      let output = (command controller)
      let current = selectedKey controller
  returnA -< (current, output)

-- Used for the command switch. The currently selected mode routes commands to
-- one of the downstream signal functions. Change the mode by pressing the 'm'
-- key.
modeSF :: forall n. (KnownNat n) => SF (Event T.KeyState) (Finite n)
modeSF = proc event' -> do
  rec let current' = increment' previous event'
      previous <- iPre 0 -< current'
  returnA -< finite current'
  where
    increment' :: Integer -> Event T.KeyState -> Integer
    increment' previous (Event e) =
      let max' = (fromInteger $ natVal $ Proxy @n)
       in case (T.keyId e, T.state e) of
            ('m', T.Pressed) -> (previous + 1) `mod` max'
            _ -> previous
    increment' previous NoEvent = previous

-- Switch commands between a group of downstream signal functions.
commandSwitchSF ::
  forall m n.
  (KnownNat n) =>
  SF (Event T.KeyState, Event (Command m)) (Finite n, V.Vector n (Event (Command m)))
commandSwitchSF = proc (keyState', command') -> do
  mode' <- modeSF @n -< keyState'
  let commands' = V.replicate NoEvent V.// [(mode', command')]
  returnA -< (mode', commands')
