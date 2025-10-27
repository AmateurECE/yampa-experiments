{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ActivationSwitch.Configuration
  ( Command (..),
    Direction (..),
    configurationSF,
    commandSwitchSF,
  )
where

import ActivationSwitch.Therapy
import qualified ActivationSwitch.Therapy as T
import Data.Finite
import Data.Proxy
import qualified Data.Vector.Sized as V
import FRP.Yampa hiding (left, right)
import GHC.TypeLits
import Unsafe.Coerce
import Prelude hiding (sequence)

data Direction = Up | Down

data Command n = Command
  { keyIndex :: Finite n,
    direction :: Direction
  }

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

modeSF :: forall n. (KnownNat n) => SF (Event T.KeyState) (Finite n)
modeSF = proc event' -> do
  rec let current' = increment' previous event'
      previous <- iPre 0 -< current'
  -- INVARIANT: increment' will never produce a Nat greater than or equal to n
  returnA -< unsafeCoerce $ current'
  where
    increment' :: Nat -> Event T.KeyState -> Nat
    increment' previous (Event e) =
      let max' = (fromInteger $ natVal $ Proxy @n)
       in case (T.keyId e, T.state e) of
            ('m', T.Pressed) -> (previous + 1) `mod` max'
            _ -> previous
    increment' previous NoEvent = previous

commandSwitchSF ::
  forall m n.
  (KnownNat n) =>
  SF (Event T.KeyState, Event (Command m)) (Finite n, V.Vector n (Event (Command m)))
commandSwitchSF = proc (keyState', command') -> do
  mode' <- modeSF @n -< keyState'
  let commands' = V.replicate NoEvent V.// [(mode', command')]
  returnA -< (mode', commands')
