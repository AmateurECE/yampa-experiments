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
import qualified Data.ByteString as BS
import Data.Foldable
import qualified Data.List as L
import FRP.Yampa hiding (event)
import Linear

initialize :: IO (Event a)
initialize = pure NoEvent

keySF :: Char -> SF ([Event T.KeyState]) T.KeyState
keySF target = proc allKeys -> do
  key <- filterByKey -< allKeys
  current <- arr $ mergeEvents -< key
  stable <- hold (T.KeyState target T.Unpressed) -< current
  returnA -< stable
  where
    filterByKey = arr (fmap $ filterE (\e -> T.keyId e == target))

controllerSF :: SF ([Event T.KeyState]) (V3 T.KeyState)
controllerSF = proc events -> do
  one <- keySF '1' -< events
  two <- keySF '2' -< events
  three <- keySF '3' -< events
  returnA -< V3 one two three

therapySF :: SF (V3 T.KeyState) T.TherapyState
therapySF = proc keyStates -> do
  rec let current = T.therapy previous $ toList keyStates
      previous <- iPre T.Inactive -< current
  returnA -< current

application :: SF (Event BS.ByteString) UIState
application = proc event -> do
  keyEvents <- S.parseKeyEventsSF -< event
  keyStates <- controllerSF -< keyEvents
  state <- therapySF -< keyStates

  (selected, commands) <- C.configurationSF 3 -< keyEvents
  (settings, current) <- P.powerLevelSF $ V3 '1' '2' '3' -< (state, commands)
  returnA -< UIState state current selected settings

data UIState = UIState
  { therapy :: T.TherapyState,
    powerLevel :: Maybe P.PowerLevel,
    selectedKey :: Int,
    powerLevels :: V3 P.PowerLevelSetting
  }
  deriving (Eq)

class Render a where
  render :: a -> String

instance Render T.TherapyState where
  render s = "Therapy: " ++ show s

instance Render P.PowerLevelSetting where
  render s = (show $ P.keyId s) ++ ": " ++ (show $ P.powerLevel s)

instance Render (Maybe P.PowerLevel) where
  render p =
    "Power Level: " ++ case p of
      Just a -> show a
      Nothing -> "Off"

select :: Int -> [String] -> [String]
select ind xs =
  let enumerated = zip xs (take (length xs) (iterate (+ 1) 0))
   in sel <$> enumerated
  where
    sel (x, i) = if i == ind then "\ESC[7m" ++ x ++ "\ESC[27m" else x

renderUI :: UIState -> IO ()
renderUI state = do
  putStrLn $ render (therapy state) ++ "\ESC[0K"
  putStrLn $ render (powerLevel state) ++ "\ESC[0K"
  putStr $ L.intercalate " " $ select (selectedKey state) $ render <$> (toList $ powerLevels state)
  putStr "\ESC[0K\ESC[2A\r"
