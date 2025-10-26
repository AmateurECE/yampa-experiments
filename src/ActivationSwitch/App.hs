{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}

module ActivationSwitch.App
  ( application,
    initialize,
    renderUI,
    T.TherapyState,
    UIState,
  )
where

import qualified ActivationSwitch.Configuration as C
import qualified ActivationSwitch.Power as P
import qualified ActivationSwitch.Switch as S
import qualified ActivationSwitch.Therapy as T
import Control.Lens hiding (set')
import qualified Data.ByteString as BS
import Data.Foldable
import qualified Data.List as L
import FRP.Yampa hiding (event)
import Linear

modeSF :: Int -> SF ([Event T.KeyState]) Int
modeSF numberOfModes = proc events -> do
  rec let increments =
            L.length $
              filter isEvent $
                (filterE $ \k -> T.keyId k == 'm' && T.state k == T.Pressed) <$> events
      let current' = (previous + increments) `mod` numberOfModes
      previous <- iPre 0 -< current'
  returnA -< current'

-- TODO: How to make this polymorphic?
commandSwitchSF ::
  SF ([Event T.KeyState], [Event C.Command]) (Int, V2 [Event C.Command])
commandSwitchSF = proc (keyStates, commands) -> do
  mode' <- modeSF 2 -< keyStates
  let commands' = set' mode' commands $ pure []
  returnA -< (mode', commands')
  where
    set' mode' = case mode' of
      0 -> set _x
      1 -> set _y
      _ -> pure id

initialize :: IO (Event a)
initialize = pure NoEvent

keySF :: Char -> SF ([Event T.KeyState]) T.KeyState
keySF target = proc allKeys -> do
  key <- filterByKey -< allKeys
  current' <- arr $ mergeEvents -< key
  stable <- hold (T.KeyState target T.Unpressed) -< current'
  returnA -< stable
  where
    filterByKey = arr (fmap $ filterE (\e -> T.keyId e == target))

keys :: V3 Char
keys = V3 '1' '2' '3'

controllerSF :: SF ([Event T.KeyState]) (V3 T.KeyState)
controllerSF = proc events -> do
  one <- keySF $ keys ^. _x -< events
  two <- keySF $ keys ^. _y -< events
  three <- keySF $ keys ^. _z -< events
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
  keyEvents <- S.parseKeyEventsSF -< event
  keyStates <- controllerSF -< keyEvents

  (selected, commands) <- C.configurationSF 3 -< keyEvents
  (mode', commands') <- commandSwitchSF -< (keyEvents, commands)
  powerLevels <- P.setPowerLevelSF $ keys -< commands' ^. _x
  enabled <- S.setEnabledSF -< commands' ^. _y

  state <- therapySF <<< enabledKeysSF -< (enabled, keyStates)
  powerLevel' <- P.showPowerLevelSF -< (powerLevels, state)

  let settings' = mkSettings selected mode' powerLevels enabled
  returnA -< UIState state (CurrentPowerLevel powerLevel') settings'
  where
    -- TODO: This sucks
    mkSettings :: Int -> Int -> V3 P.PowerLevelSetting -> V3 Bool -> SwitchSettings
    mkSettings selectedKey' mode' powerLevels enabled' =
      let values' = case mode' of
            0 -> Left $ P.powerLevel <$> powerLevels
            _ -> Right $ enabled'
       in SwitchSettings selectedKey' keys values'

newtype CurrentPowerLevel = CurrentPowerLevel {current :: Maybe P.PowerLevel}
  deriving (Eq)

data SwitchSettings = SwitchSettings
  { selectedKey :: Int,
    keyIds :: V3 Char,
    values :: Either (V3 P.PowerLevel) (V3 Bool)
  }
  deriving (Eq)

data UIState = UIState
  { therapy :: T.TherapyState,
    powerLevel :: CurrentPowerLevel,
    settings :: SwitchSettings
  }
  deriving (Eq)

class Render a where
  render :: a -> String

instance Render T.TherapyState where
  render s = "Therapy: " ++ show s

instance Render P.PowerLevel where
  render = show

instance Render Bool where
  render True = "Enabled"
  render False = "Disabled"

instance Render CurrentPowerLevel where
  render p =
    "Power Level: " ++ case current p of
      Just a -> show a
      Nothing -> "Off"

instance Render SwitchSettings where
  render s =
    L.intercalate " " $
      select $
        fmap (uncurry label') $
          zip (toList $ keyIds s) $
            either render' render' (values s)
    where
      label' :: Char -> String -> String
      label' k v = [k] ++ ": " ++ v

      render' :: (Render a) => (V3 a) -> [String]
      render' = (fmap render) . toList

      select :: [String] -> [String]
      select xs =
        let enumerated = zip xs (take (length xs) (iterate (+ 1) 0))
         in sel <$> enumerated

      sel (x, i) = if i == (selectedKey s) then "\ESC[7m" ++ x ++ "\ESC[27m" else x

renderUI :: UIState -> IO ()
renderUI state = do
  putStrLn $ render (therapy state) ++ "\ESC[0K"
  putStrLn $ render (powerLevel state) ++ "\ESC[0K"
  putStr $ render (settings state)
  putStr "\ESC[0K\ESC[2A\r"
