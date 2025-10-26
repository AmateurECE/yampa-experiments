{-# LANGUAGE Arrows #-}

module ActivationSwitch.Power
  ( setPowerLevelSF,
    showPowerLevelSF,
    PowerLevel (..),
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

-- Get the default power levels for the set of keys
defaults :: V3 PowerLevel
defaults = V3 Medium High Low

change :: Direction -> PowerLevel -> PowerLevel
change Down Low = Low
change Up Low = Medium
change Down Medium = Low
change Up Medium = High
change Down High = Medium
change Up High = High

-- Set the power levels from a command
set :: V3 PowerLevel -> Event Command -> V3 PowerLevel
set levels NoEvent = levels
set levels (Event c) = applyCommand levels
  where
    applyCommand :: V3 PowerLevel -> V3 PowerLevel
    applyCommand = case keyIndex c of
      0 -> (& _x %~ update')
      1 -> (& _y %~ update')
      2 -> (& _z %~ update')
      _ -> id

    update' = change (direction c)

-- Get the active power level based on the state of therapy and the power level
-- settings.
get :: V3 Char -> V3 PowerLevel -> TherapyState -> Maybe PowerLevel
get keys levels (Active c) = snd <$> find ((== c) . fst) (toList $ liftA2 (,) keys levels)
get _ _ _ = Nothing

showPowerLevelSF :: V3 Char -> SF (V3 PowerLevel, TherapyState) (Maybe PowerLevel)
showPowerLevelSF keys = arr $ uncurry $ (get keys)

setPowerLevelSF :: SF (Event Command) (V3 PowerLevel)
setPowerLevelSF = proc command -> do
  rec let current = set previous command
      previous <- iPre $ defaults -< current
  returnA -< current
