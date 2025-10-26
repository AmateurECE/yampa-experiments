{-# LANGUAGE Arrows #-}

module ActivationSwitch.Power
  ( setPowerLevelSF,
    showPowerLevelSF,
    PowerLevel (..),
    PowerLevelSetting (..),
  )
where

import ActivationSwitch.Configuration
import ActivationSwitch.Therapy hiding (keyId, therapy)
import Control.Lens hiding (levels, set)
import Data.Foldable
import FRP.Yampa
import Linear
import Prelude hiding (sequence)

data PowerLevel = Low | Medium | High
  deriving (Show, Eq)

data PowerLevelSetting = PowerLevelSetting
  { keyId :: Char,
    powerLevel :: PowerLevel
  }
  deriving (Eq)

-- Get the default power levels for the set of keys
defaults :: V3 Char -> V3 PowerLevelSetting
defaults keys = liftI2 PowerLevelSetting keys (V3 Medium High Low)

change :: PowerLevel -> Direction -> PowerLevel
change Low Down = Low
change Low Up = Medium
change Medium Down = Low
change Medium Up = High
change High Down = Medium
change High Up = High

-- Set the power levels from a command
set :: V3 PowerLevelSetting -> Event Command -> V3 PowerLevelSetting
set levels NoEvent = levels
set levels (Event c) = applyCommand levels
  where
    applyCommand :: V3 PowerLevelSetting -> V3 PowerLevelSetting
    applyCommand = case keyIndex c of
      0 -> (& _x %~ update)
      1 -> (& _y %~ update)
      2 -> (& _z %~ update)
      _ -> id

    update s = s {powerLevel = change (powerLevel s) (direction c)}

-- Get the active power level based on the state of therapy and the power level
-- settings.
get :: V3 PowerLevelSetting -> TherapyState -> Maybe PowerLevel
get v (Active c) = powerLevel <$> find ((== c) . keyId) (toList v)
get _ _ = Nothing

showPowerLevelSF :: SF (V3 PowerLevelSetting, TherapyState) (Maybe PowerLevel)
showPowerLevelSF = arr $ uncurry $ get

setPowerLevelSF :: V3 Char -> SF (Event Command) (V3 PowerLevelSetting)
setPowerLevelSF keys = proc command -> do
  rec let current = set previous command
      previous <- iPre $ defaults keys -< current
  returnA -< current
