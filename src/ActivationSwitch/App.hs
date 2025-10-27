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
import Data.Finite
import Data.Foldable
import qualified Data.Vector.Sized as V
import FRP.Yampa hiding (event)
import GHC.TypeLits

-- TODO: Refactor wishlist:
-- 1. Polymorphic "setXSF" function
-- 2. Polymorphism over the linear types?
-- 4. Check the rest of the TODOs
-- 5. Some of this configuration garbage in Configuration.hs

initialize :: IO (Event a)
initialize = pure NoEvent

keySF :: Char -> SF (Event T.KeyState) T.KeyState
keySF target = proc allKeys -> do
  key <- filterByKey -< allKeys
  stable <- hold (T.KeyState target T.Unpressed) -< key
  returnA -< stable
  where
    filterByKey = arr (filterE (\e -> T.keyId e == target))

type NumberOfKeys = 3 :: Nat

type Bus = V.Vector NumberOfKeys

keys :: Bus Char
keys = V.fromTuple ('1', '2', '3')

-- The default power levels for the set of keys
defaults :: Bus P.PowerLevel
defaults = V.fromTuple (P.Medium, P.High, P.Low)

controllerSF :: SF (Event T.KeyState) (Bus T.KeyState)
controllerSF = proc event -> do
  one <- keySF $ keys `V.index` 0 -< event
  two <- keySF $ keys `V.index` 1 -< event
  three <- keySF $ keys `V.index` 2 -< event
  returnA -< V.fromTuple (one, two, three)

enabledKeysSF :: SF (Bus Bool, Bus T.KeyState) (Bus T.KeyState)
enabledKeysSF = arr $ uncurry $ liftA2 shunt'
  where
    shunt' :: Bool -> T.KeyState -> T.KeyState
    shunt' True s = s
    shunt' False s = s {T.state = T.Unpressed}

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

  state <- therapySF <<< enabledKeysSF -< (enabled, keyStates)
  powerLevel' <- P.showPowerLevelSF $ keys -< (powerLevels, state)

  let settings' = mkSettings selected (fromInteger $ getFinite mode') powerLevels enabled
  returnA -< UIState state (CurrentPowerLevel powerLevel') settings'
  where
    mkSettings :: Finite n -> Finite 2 -> Bus P.PowerLevel -> Bus Bool -> SwitchSettings NumberOfKeys
    mkSettings selectedKey' mode' powerLevels enabled' =
      let values' = case mode' of
            0 -> Left $ powerLevels
            _ -> Right $ enabled'
       in SwitchSettings (fromInteger $ getFinite selectedKey') keys values'
