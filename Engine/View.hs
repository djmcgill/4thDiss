{-# LANGUAGE TemplateHaskell #-}

module Engine.View where

import Netwire.SFML
import Netwire.SFML.Wires

import Control.Lens
import Control.Wire
import Data.Monoid
import Numeric.LinearAlgebra
import Prelude hiding ((.), id)
import SFML.Window (KeyCode(..))

data WorldView = WorldView {
  _eyeLocation    :: Vector Double,
  _eyeVerticalTilt  :: Double,
  _eyeHorizontalTilt  :: Double}
makeLenses ''WorldView

translateView :: Vector Double -> WorldView -> WorldView
translateView vector = eyeLocation +~ vector

tiltView :: Double -> Double -> WorldView -> WorldView
tiltView h v = (eyeHorizontalTilt +~ h)
       . (eyeVerticalTilt   +~ v)

initialView :: WorldView
initialView = WorldView (fromList [0,0,15]) 0 0

-- | Given an initial worldView, make a wire that changes it for keyboard input
viewToWire :: (Monoid e, Monad m) => WorldView -> Wire e m Input WorldView
viewToWire view = accumT1 (\t x f -> f t x) view . (arr moveView . keyEvent <|> pure (const id))
  where
  moveView (key,_,_,True,_) dt = case key of
    KeyUp  -> tiltView  0   (-dt')
    KeyDown  -> tiltView  0   dt'
    KeyLeft  -> tiltView (-dt')  0
    KeyRight -> tiltView   dt'   0
    _    -> id
    where dt' = dt*150
  moveView (key,_,_,False,_) dt = case key of
    KeyUp  -> translateView $ fromList [  0 , dt',0]
    KeyDown  -> translateView $ fromList [  0 ,-dt',0]
    KeyLeft  -> translateView $ fromList [-dt',  0 ,0]
    KeyRight -> translateView $ fromList [ dt',  0 ,0]
    _    -> id
    where dt' = dt*100