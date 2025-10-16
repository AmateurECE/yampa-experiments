{-# LANGUAGE Arrows #-}

module ActivationSwitch.App
  ( application,
    initialize,
    renderUI,
    TherapyState,
  )
where

import ActivationSwitch.Switch
import ActivationSwitch.Therapy
import qualified Data.ByteString as BS
import FRP.Yampa

initialize :: IO (Event a)
initialize = pure NoEvent

keySF :: Char -> SF ([Event KeyState]) KeyState
keySF target = proc allKeys -> do
  key <- filterByKey -< allKeys
  current <- arr $ mergeEvents -< key
  stable <- hold (KeyState target Unpressed) -< current
  returnA -< stable
  where
    filterByKey = arr (fmap $ filterE (\e -> keyId e == target))

controller :: SF ([Event KeyState]) [KeyState]
controller = proc events -> do
  one <- keySF '1' -< events
  two <- keySF '2' -< events
  three <- keySF '3' -< events
  returnA -< [one, two, three]

therapySF :: SF [KeyState] TherapyState
therapySF = proc keyStates -> do
  rec let current = therapy previous keyStates
      previous <- iPre Inactive -< current
  returnA -< current

application :: SF (Event BS.ByteString) TherapyState
application = therapySF <<< controller <<< parseKeyEventsSF

renderUI :: TherapyState -> IO ()
renderUI therapyState = putStr $ "\rTherapy: " ++ show therapyState ++ "\ESC[0K"
