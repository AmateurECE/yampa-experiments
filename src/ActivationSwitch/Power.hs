{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}

module ActivationSwitch.Power
  ( setPowerLevelSF,
    showPowerLevelSF,
    PowerLevel (..),
  )
where

import ActivationSwitch.Configuration
import ActivationSwitch.Therapy
import Data.Foldable
import qualified Data.Vector.Sized as V
import FRP.Yampa
import GHC.TypeLits

data PowerLevel = Low | Medium | High
  deriving (Show, Eq)

change :: Direction -> PowerLevel -> PowerLevel
change Down Low = Low
change Up Low = Medium
change Down Medium = Low
change Up Medium = High
change Down High = Medium
change Up High = High

-- Get the active power level based on the state of therapy and the power level
-- settings.
get ::
  forall n.
  (KnownNat n) =>
  V.Vector n Char ->
  V.Vector n PowerLevel ->
  TherapyState ->
  Maybe PowerLevel
get keys levels (Active c) = snd <$> find ((== c) . fst) (toList $ liftA2 (,) keys levels)
get _ _ _ = Nothing

-- Show the current power level given the state of therapy and the configured
-- power level settings.
showPowerLevelSF ::
  forall n.
  (KnownNat n) =>
  V.Vector n Char ->
  SF (V.Vector n PowerLevel, TherapyState) (Maybe PowerLevel)
showPowerLevelSF keys = arr $ uncurry $ (get keys)

-- Set the power level of switches in response to commands.
setPowerLevelSF ::
  V.Vector n PowerLevel ->
  SF (Event (Command n)) (V.Vector n PowerLevel)
setPowerLevelSF defaults = setParameterSF (set change) defaults
