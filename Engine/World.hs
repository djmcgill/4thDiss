{-# LANGUAGE Arrows, TemplateHaskell #-}

module Engine.World where

import Engine.RigidBody
import Netwire.SFML

import Control.Applicative ((<$>))
import Control.Monad.Identity (Identity)
import Control.Lens
import Control.Wire
import Graphics.Rendering.OpenGL (Vertex3(..), Vector3(..))
import Numeric.LinearAlgebra
import Prelude hiding (id, (.))
import SFML.Graphics (RenderWindow, clearRenderWindow, white)
import SFML.Window (display, SFEvent(SFEvtClosed, SFEvtKeyPressed), KeyCode(..))

data Object = Object BasicObject (Input -> (Acceleration, PostUpdateFun)) RigidBody

data BasicObject
    = Sphere {radius :: Double}
    | Cuboid {xlen :: Double
             ,ylen :: Double
             ,zlen :: Double}

data World = World { -- TODO: split worldView into its own constructor
    _objects :: [Object],
    _worldView    :: WorldView
}

data WorldView = WorldView {
    _eyeLocation :: Vector Double,
    _eyeForward  :: Vector Double,
    _eyeUp       :: Vector Double
}

makeLenses ''World
makeLenses ''WorldView

rotateView :: Double -> World -> World
rotateView rads = worldView.eyeUp %~ undefined -- rotate a vector around the axis of eyeForward

translateView :: Vector Double -> World -> World
translateView vector = worldView.eyeLocation %~ (+) vector

-- rotate both eyeForward and eyeUp about the relative x and y axes
panView :: Double -> Double -> World -> World
panView x y = worldView.eyeForward %~ undefined
            . worldView.eyeUp      %~ undefined

initialCube :: Object
initialCube = Object basicCuboid forceFunction (createUniformBody basicCuboid & x .~ startingPos)
    where
    forceFunction :: Input -> (Acceleration, PostUpdateFun)
    forceFunction _ = (\_ _ -> (fromList [0,0,-10], fromList [0,0,0]), bounce)

    -- if z <= 1, reverse its vertical speed
    bounce :: RigidBody -> RigidBody
    bounce body | (body^.x) @> 2 <= 1 = p %~ mapVector negate $ body
                | otherwise           = body

    basicCuboid = Cuboid 1 1 1
    startingPos = fromList [0,0,10]


initialWorld :: World
initialWorld = World [initialCube] initialView

initialView :: WorldView
initialView = WorldView (fromList [0,0,15]) (fromList [0,0,-1]) (fromList [1,0,0])

pureWorld :: Wire () IO Input (Output World)
pureWorld = proc mEvent -> do
    world <- stepWorld initialWorld -< mEvent
    let (draw, exit) = case mEvent of
            Just SFEvtClosed                         -> (False, True)
            Just (SFEvtKeyPressed KeyEscape _ _ _ _) -> (False, True)
            _                                        -> (True, False)
    returnA -< Output world draw exit

createUniformBody :: BasicObject -> RigidBody
createUniformBody (Cuboid x0 y0 z0) = updateStateVars $ RigidBody {
    _mass     = mass',
    _iBody    = iBody',
    _iBodyInv = inv iBody',

    _x        = zeroVector,
    _r        = zeroMatrix,
    _p        = zeroVector,
    _l        = zeroVector,

    _iInv     = zeroMatrix,
    _v        = zeroVector,
    _omega    = zeroVector}
    where
    mass' = x0*y0*z0
    iBody' = diag $ fromList $ map (*(mass'/12)) [y0^2 + z0^2, x0^2 + z0^2, x0^2 + y0^2]
createUniformBody (Sphere r0) = updateStateVars $ RigidBody {
    _mass     = mass',
    _iBody    = iBody',
    _iBodyInv = inv iBody',

    _x        = zeroVector,
    _r        = zeroMatrix,
    _p        = zeroVector,
    _l        = zeroVector,

    _iInv     = zeroMatrix,
    _v        = zeroVector,
    _omega    = zeroVector}
    where
    mass' = (4/3)*pi*r0^3
    iBody' = undefined
    {-
    XXX: these are almost certainly wrong, there are some off-diag terms probably
    I_xx = 10/15 * pi * r_0^5
    I_yy = 8 /15 * pi * r_0^5
    I_zz = 2 /5  * pi * r_0^5
    -}

zeroMatrix :: Matrix Double
zeroMatrix = konst 0 (3,3)
zeroVector :: Vector Double
zeroVector = fromList [0,0,0]

-- | Given an initial object, turn it into a Wire that holds its state
objectToWire :: Monad m => Object -> Wire e m Input Object
objectToWire (Object basic forces rigid) = Object basic forces <$> rigidObject rigid . arr forces

-- | Given an initial worldView, make a wire that changes it for keyboard input
viewToWire :: Monad m => WorldView -> Wire e m Input WorldView
viewToWire = arr . const -- XXX: always returns the initial view

stepWorld :: Monad m => World -> Wire e m Input World
stepWorld initialWorld = proc input -> do
    worldView' <- viewToWire (initialWorld^.worldView) -< input
    objects' <- multicast (map objectToWire (initialWorld^.objects)) -< input
    returnA -< (World objects' worldView')