{-# LANGUAGE Arrows #-}

module ActivationSwitch.App
  ( application,
    initialize,
    renderUI,
    T.TherapyState,
  )
where

import qualified ActivationSwitch.Switch as S
import qualified ActivationSwitch.Therapy as T
import qualified Data.ByteString as BS
import Data.Foldable
import FRP.Yampa
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

application :: SF (Event BS.ByteString) T.TherapyState
application = therapySF <<< controllerSF <<< S.parseKeyEventsSF

renderUI :: T.TherapyState -> IO ()
renderUI therapyState = putStr $ "\rTherapy: " ++ show therapyState ++ "\ESC[0K"
