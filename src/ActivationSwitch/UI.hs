module ActivationSwitch.UI
  ( CurrentPowerLevel (..),
    SwitchSettings (..),
    UIState (..),
    renderUI,
  )
where

import qualified ActivationSwitch.Power as P
import qualified ActivationSwitch.Therapy as T
import Data.Foldable
import qualified Data.List as L
import Linear

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
