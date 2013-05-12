{-# LANGUAGE Arrows, TemplateHaskell #-}

module Engine.World where

import Engine.Objects
import Engine.View
import Netwire.SFML

import Control.Lens
import Control.Wire
import Data.Monoid (Monoid)
import Numeric.LinearAlgebra
import Prelude hiding (id, (.))
import SFML.Window (SFEvent(..), KeyCode(..))

import Data.Time.Clock (getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

data World = World {
  _objects   :: [Object],
  _worldView :: WorldView}
makeLenses ''World

initialWorld :: World
initialWorld = World [initialCube, initialCube2] initialView

worldWire :: Wire () IO Input (Output World)
worldWire = proc mEvent -> do
  world <- worldToWire initialWorld -< mEvent
  let
    (draw, exit) = case mEvent of
      Just SFEvtClosed                         -> (False, True )
      Just (SFEvtKeyPressed KeyEscape _ _ _ _) -> (False, True )
      _                                        -> (True , False)
  returnA -< Output world draw exit

worldToWire :: (Monoid e, Monad m) => World -> Wire e m Input World
worldToWire initialWorld = proc input -> do
  worldView' <- viewToWire (initialWorld^.worldView)         -< input
  objects'   <- multicast (map objectToWire (initialWorld^.objects)) -< input
  returnA -< World objects' worldView'
