{-# LANGUAGE Arrows #-}

module ActivationSwitch.Power
  ( powerLevelSF,
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

change :: PowerLevel -> Direction -> PowerLevel
change Low Down = Low
change Low Up = Medium
change Medium Down = Low
change Medium Up = High
change High Down = Medium
change High Up = High

defaults :: V3 Char -> V3 PowerLevelSetting
defaults keys = liftI2 PowerLevelSetting keys (V3 Medium High Low)

applyCommand :: Command -> V3 PowerLevelSetting -> V3 PowerLevelSetting
applyCommand c = case keyIndex c of
  0 -> (& _x %~ update)
  1 -> (& _y %~ update)
  2 -> (& _z %~ update)
  _ -> id
  where
    update s = s {powerLevel = change (powerLevel s) (direction c)}

set :: V3 PowerLevelSetting -> [Event Command] -> V3 PowerLevelSetting
set = foldl step
  where
    step levels (Event c) = applyCommand c levels
    step levels NoEvent = levels

get :: V3 PowerLevelSetting -> TherapyState -> Maybe PowerLevel
get v (Active c) = powerLevel <$> find ((== c) . keyId) (toList v)
get _ _ = Nothing

powerLevelSF ::
  V3 Char ->
  SF (TherapyState, [Event Command]) (V3 PowerLevelSetting, Maybe PowerLevel)
powerLevelSF keys = proc (therapy, commands) -> do
  rec let current = set previous commands
      previous <- iPre $ defaults keys -< current
      let output = get current therapy
  returnA -< (current, output)
