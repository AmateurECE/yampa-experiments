{-# OPTIONS_GHC -Wno-x-partial #-}

module ActivationSwitch.Therapy
  ( therapy,
    TherapyState (..),
    SwitchState (..),
    KeyState (..),
  )
where

data SwitchState = Unpressed | Pressed deriving (Show, Eq)

data KeyState = KeyState
  { keyId :: Char,
    state :: SwitchState
  }
  deriving (Show)

data TherapyState = Active Char | Inactive | Blocked

instance Show TherapyState where
  show (Active _) = "Active"
  show _ = "Inactive"

pressedSwitches :: [KeyState] -> [KeyState]
pressedSwitches = filter (\s -> state s == Pressed)

therapy :: TherapyState -> [KeyState] -> TherapyState
therapy Inactive states = case pressedSwitches states of
  [one] -> Active $ keyId one
  _ -> Inactive
therapy (Active a) states =
  -- INVARIANT: keyIds are constant. This makes the application sound, but also
  -- ensures head does not throw in the following expression.
  let currentState = state $ head $ filter (\s -> keyId s == a) states
   in case currentState of
        Pressed -> Active a
        Unpressed -> Blocked
therapy Blocked states = case pressedSwitches states of
  [] -> Inactive
  _ -> Blocked
