{-# LANGUAGE Arrows, TemplateHaskell #-}

module Engine.World where

import Engine.Objects
import Engine.View
import Netwire.SFML

import Control.Lens
import Control.Wire
import Data.Monoid (Monoid)
import Prelude hiding (id, (.))
import SFML.Window (SFEvent(SFEvtClosed, SFEvtKeyPressed), KeyCode(..))


data World = World {
    _objects   :: [Object],
    _worldView :: WorldView
}
makeLenses ''World

initialWorld :: World
initialWorld = World [initialCube] initialView

-- TODO: rename
pureWorld :: Wire () IO Input (Output World)
pureWorld = proc mEvent -> do
    world <- stepWorld initialWorld -< mEvent
    let (draw, exit) = case mEvent of
            Just SFEvtClosed                         -> (False, True)
            Just (SFEvtKeyPressed KeyEscape _ _ _ _) -> (False, True)
            _                                        -> (True, False)
    returnA -< Output world draw exit

stepWorld :: (Monoid e, Monad m) => World -> Wire e m Input World
stepWorld initialWorld = proc input -> do
    worldView' <- viewToWire (initialWorld^.worldView) -< input
    objects' <- multicast (map objectToWire (initialWorld^.objects)) -< input
    returnA -< (World objects' worldView')
