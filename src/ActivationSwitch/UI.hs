module ActivationSwitch.UI
  ( mkUIState,
    UIState (..),
    renderUI,
  )
where

import qualified ActivationSwitch.Therapy as T

newtype UIState = UIState
  { therapy :: T.TherapyState
  }
  deriving (Eq)

class Render a where
  render :: a -> String

instance Render T.TherapyState where
  render s = "Therapy: " ++ show s

mkUIState :: T.TherapyState -> UIState
mkUIState therapy' = UIState therapy'

renderUI :: UIState -> IO ()
renderUI state = do
  putStr $ render (therapy state) ++ "\ESC[0K\r"
