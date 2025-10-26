{-# LANGUAGE Arrows #-}

module ActivationSwitch.App
  ( application,
    initialize,
    renderUI,
    UIState,
    T.TherapyState,
  )
where

import qualified ActivationSwitch.Configuration as C
import qualified ActivationSwitch.Power as P
import qualified ActivationSwitch.Switch as S
import qualified ActivationSwitch.Therapy as T
import ActivationSwitch.UI
import Control.Lens hiding (set')
import qualified Data.ByteString as BS
import Data.Foldable
import FRP.Yampa hiding (event)
import GHC.TypeLits
import Linear

-- TODO: Refactor wishlist:
-- 1. Polymorphic "setXSF" function
-- 2. Polymorphism over the linear types?
-- 4. Check the rest of the TODOs
-- 5. Some of this configuration garbage in Configuration.hs

modeSF :: Nat -> SF (Event T.KeyState) Nat
modeSF numberOfModes = proc event -> do
  rec let current' = increment' previous event
      previous <- iPre 0 -< current'
  returnA -< current'
  where
    increment' :: Nat -> Event T.KeyState -> Nat
    increment' previous (Event e) = case (T.keyId e, T.state e) of
      ('m', T.Pressed) -> (previous + 1) `mod` numberOfModes
      _ -> previous
    increment' previous NoEvent = previous

-- TODO: How to make this polymorphic?
commandSwitchSF ::
  SF (Event T.KeyState, Event C.Command) (Nat, V2 (Event C.Command))
commandSwitchSF = proc (keyState', commands) -> do
  mode' <- modeSF 2 -< keyState'
  let commands' = set' mode' commands $ pure NoEvent
  returnA -< (mode', commands')
  where
    set' mode' = case mode' of
      0 -> set _x
      1 -> set _y
      _ -> pure id

initialize :: IO (Event a)
initialize = pure NoEvent

keySF :: Char -> SF (Event T.KeyState) T.KeyState
keySF target = proc allKeys -> do
  key <- filterByKey -< allKeys
  stable <- hold (T.KeyState target T.Unpressed) -< key
  returnA -< stable
  where
    filterByKey = arr (filterE (\e -> T.keyId e == target))

keys :: V3 Char
keys = V3 '1' '2' '3'

controllerSF :: SF (Event T.KeyState) (V3 T.KeyState)
controllerSF = proc event -> do
  one <- keySF $ keys ^. _x -< event
  two <- keySF $ keys ^. _y -< event
  three <- keySF $ keys ^. _z -< event
  returnA -< V3 one two three

enabledKeysSF :: SF (V3 Bool, V3 T.KeyState) (V3 T.KeyState)
enabledKeysSF = arr $ uncurry $ liftA2 shunt'
  where
    shunt' :: Bool -> T.KeyState -> T.KeyState
    shunt' True s = s
    shunt' False s = s {T.state = T.Unpressed}

therapySF :: SF (V3 T.KeyState) T.TherapyState
therapySF = proc keyStates -> do
  rec let current' = T.therapy previous $ toList keyStates
      previous <- iPre T.Inactive -< current'
  returnA -< current'

application :: SF (Event BS.ByteString) UIState
application = proc event -> do
  keyEvent <- S.parseKeyEventsSF -< event
  keyStates <- controllerSF -< keyEvent

  (selected, command) <- C.configurationSF 3 -< keyEvent
  (mode', command') <- commandSwitchSF -< (keyEvent, command)
  powerLevels <- P.setPowerLevelSF -< command' ^. _x
  enabled <- S.setEnabledSF -< command' ^. _y

  state <- therapySF <<< enabledKeysSF -< (enabled, keyStates)
  powerLevel' <- P.showPowerLevelSF $ keys -< (powerLevels, state)

  let settings' = mkSettings selected mode' powerLevels enabled
  returnA -< UIState state (CurrentPowerLevel powerLevel') settings'
  where
    mkSettings :: Int -> Nat -> V3 P.PowerLevel -> V3 Bool -> SwitchSettings
    mkSettings selectedKey' mode' powerLevels enabled' =
      let values' = case mode' of
            0 -> Left $ powerLevels
            _ -> Right $ enabled'
       in SwitchSettings selectedKey' keys values'
