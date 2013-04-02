{-# LANGUAGE Arrows, TemplateHaskell #-}

module Engine.World where

import Engine.RigidBody
import Netwire.SFML
import Netwire.SFML.Wires

import Control.Applicative ((<$>))
import Control.Monad.Identity (Identity)
import Control.Lens
import Control.Wire
import Data.Monoid (Monoid)
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
    _eyeLocation        :: Vector Double,
    _eyeVerticalTilt    :: Double,
    _eyeHorizontalTilt  :: Double
}

makeLenses ''World
makeLenses ''WorldView

translateView :: Vector Double -> WorldView -> WorldView
translateView vector = eyeLocation +~ (scale translateSpeed vector)
    where translateSpeed = 100

tiltView :: Double -> Double -> WorldView -> WorldView
tiltView h v = (eyeHorizontalTilt +~ h*tiltSpeed)
             . (eyeVerticalTilt   +~ v*tiltSpeed)
    where tiltSpeed = 100

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
initialView = WorldView (fromList [0,0,15]) 0 0

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
    iBody' = diag . scale (mass'/12) $ fromList [y0^2 + z0^2, x0^2 + z0^2, x0^2 + y0^2]
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
viewToWire :: (Monoid e, Monad m) => WorldView -> Wire e m Input WorldView
viewToWire initialView = accumT1 (\t x f -> f t x) initialView . (arr moveView . keyEvent <|> pure (const id))
    where
    moveView (key,_,_,True,_) dt = case key of
        KeyUp    -> tiltView    0    dt
        KeyDown  -> tiltView    0  (-dt)
        KeyLeft  -> tiltView   dt     0
        KeyRight -> tiltView (-dt)    0
        _        -> id
    moveView (key,_,_,False,_) dt = case key of
        KeyUp    -> translateView $ fromList [  0,-dt,0]
        KeyDown  -> translateView $ fromList [  0, dt,0]
        KeyLeft  -> translateView $ fromList [ dt,  0,0]
        KeyRight -> translateView $ fromList [-dt,  0,0]
        _        -> id

stepWorld :: (Monoid e, Monad m) => World -> Wire e m Input World
stepWorld initialWorld = proc input -> do
    worldView' <- viewToWire (initialWorld^.worldView) -< input
    objects' <- multicast (map objectToWire (initialWorld^.objects)) -< input
    returnA -< (World objects' worldView')