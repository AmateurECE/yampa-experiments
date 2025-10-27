{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Data.ByteString as BS
import Data.Foldable
import qualified Data.Vector.Sized as V
import FRP.Yampa hiding (event)
import GHC.TypeLits

type NumberOfKeys = 3 :: Nat

type Bus = V.Vector NumberOfKeys

keys :: Bus Char
keys = V.fromTuple ('1', '2', '3')

-- The default power levels for the set of keys
defaults :: Bus P.PowerLevel
defaults = V.fromTuple (P.Medium, P.High, P.Low)

initialize :: IO (Event a)
initialize = pure NoEvent

controllerSF :: SF (Event T.KeyState) (Bus T.KeyState)
controllerSF = proc event -> do
  one <- S.keySF $ keys `V.index` 0 -< event
  two <- S.keySF $ keys `V.index` 1 -< event
  three <- S.keySF $ keys `V.index` 2 -< event
  returnA -< V.fromTuple (one, two, three)

therapySF :: SF (Bus T.KeyState) T.TherapyState
therapySF = proc keyStates -> do
  rec let current' = T.therapy previous $ toList keyStates
      previous <- iPre T.Inactive -< current'
  returnA -< current'

application :: SF (Event BS.ByteString) (UIState NumberOfKeys)
application = proc event -> do
  keyEvent <- S.parseKeyEventsSF -< event
  keyStates <- controllerSF -< keyEvent

  (selected, command) <- C.configurationSF @NumberOfKeys -< keyEvent
  (mode', command') <- C.commandSwitchSF @NumberOfKeys @2 -< (keyEvent, command)
  powerLevels <- P.setPowerLevelSF defaults -< command' `V.index` 0
  enabled <- S.setEnabledSF -< command' `V.index` 1

  state <- therapySF <<< S.enabledKeysSF -< (enabled, keyStates)
  powerLevel' <- P.showPowerLevelSF $ keys -< (powerLevels, state)

  let settings' = mkSettings keys selected mode' powerLevels enabled
  returnA -< mkUIState state powerLevel' settings'
