{-# LANGUAGE Arrows, TemplateHaskell #-}

-- TODO:
--     simple physics objects
--         bouncing ball example
--     renderer
--     collisions
--     maybe just get rid of lens?

module Engine.RigidBody where

import Debug.Trace

import Control.Lens
import Control.Monad.Identity (Identity)
import Control.Wire hiding (loop, force)
import Numeric.GSL.ODE
import Numeric.LinearAlgebra
import Prelude hiding ((.), id)

type Acceleration = RigidBody -> Time -> (Force, Torque)

zeroAcc :: Acceleration
zeroAcc _ _ = (zeroV, zeroV)
    where zeroV = fromList [0,0,0]

type Force = Vector Double
type Torque = Vector Double
type PostUpdateFun = RigidBody -> RigidBody

data RigidBody = RigidBody {
    -- Constant quantities
    _mass      :: Double,        -- mass M
    _iBody     :: Matrix Double, -- I_body
    _iBodyInv  :: Matrix Double, -- (I_body)^(-1)

    -- State variables
    _x         :: Vector Double, -- x(t)
    _r         :: Matrix Double, -- R(t) TODO: quaternions instead of matrices
    _p         :: Vector Double, -- P(t)
    _l         :: Vector Double, -- L(t)

    -- Derived quantities (auxiliary variables)
    _iInv      :: Matrix Double, -- I^(-1)(t)
    _v         :: Vector Double, -- v(t)
    _omega     :: Vector Double  -- \omega(t)
}

data RigidBodyDiff = RigidBodyDiff {
    _dxdt :: Vector Double, -- d/dt (x(t)) = v(t)
    _rDot :: Matrix Double, -- Rdot(t)     = star(omega(t))R(t)
    _dpdt :: Vector Double, -- d/dt (P(t)) = F(t)
    _dldt :: Vector Double  -- d/dt (L(t)) = tau(t)
}

makeLenses ''RigidBody
makeLenses ''RigidBodyDiff

-- TODO: instead of just an acc, allow also to set velocity(s) or position(s)
rigidObject :: Monad m => RigidBody -> Wire e m (Acceleration, PostUpdateFun) RigidBody
rigidObject initialBody = mkState initialBody rigidObject' . withTime
    where
    rigidObject' :: Time -> ((Acceleration, PostUpdateFun, Time), RigidBody) -> (Either e RigidBody, RigidBody)
    rigidObject' t ((acc, postUpdate, dt), body) =
        let body' = postUpdate (updateStateVars (ode body t (t+dt) acc))
        in  (Right body', body')

    withTime :: Monad m => Wire e m (a, b) (a, b, Time)
    withTime = arr (\((a,f),t) -> (a,f,t)) . (id &&& dtime)


updateStateVars :: RigidBody -> RigidBody
updateStateVars body =
    (v     .~ body^.p / realToFrac (body^.mass))
  . (iInv  .~ body^.r <> body^.iBodyInv <> trans (body^.r))
  . (omega .~ body^.iInv <> body^.l)
  $ body

dydt :: Acceleration -> RigidBody -> Time -> RigidBodyDiff
dydt acc body t = RigidBodyDiff (body^.v) rDot' force torque
    where
    (force, torque) = acc body t
    rDot'           = star (body^.omega) <> body^.r

ode :: RigidBody
    -> Time
    -> Time
    -> Acceleration
    -> RigidBody
ode body start end accFun = updateRigidBody body . (!! 1) . toRows $ matSoln
    where
    matSoln = odeSolveV RKf45 step eps eps dydt'' Nothing (rigidBodyToVec body) times

    eps = 1.49012e-08 -- sqrt (double machine precision)

    step = (end - start)/100

    -- convert between vector and rigidbody
    -- TODO: check this
    dydt' :: Double -> Vector Double -> Vector Double
    dydt' t vec = rigidBodyDiffToVec $ dydt accFun (updateRigidBody body vec) t

    -- convert between the range (start, end) to (0, end - start)
    dydt'' = dydt' . subtract start

    -- TODO: why does ode need multiple times?
    --- XXX: BUG HERE, intermittent crashes
    times = fromList [0, end - start]

star :: Vector Double -> Matrix Double
star a = (3><3)
    [       0 ,-(a @> 2),  a @> 1 ,
       a @> 2 ,       0 ,-(a @> 0),
     -(a @> 1),  a @> 0 ,       0 ]

-- TODO: is this a lens isomorphism or something?
-- XXX: is toRows okay?
rigidBodyToVec :: RigidBody -> Vector Double
rigidBodyToVec body' = join $ body'^.x : toRows (body'^.r) ++ [body'^.p, body'^.l]

rigidBodyDiffToVec :: RigidBodyDiff -> Vector Double
rigidBodyDiffToVec diff = join $ diff^.dxdt : toRows (diff^.rDot) ++ [diff^.dpdt, diff^.dldt]

updateRigidBody :: RigidBody -> Vector Double -> RigidBody
updateRigidBody body vec =
    updateStateVars
    . (x .~ x')
    . (r .~ reshape 3 r')
    . (p .~ p')
    . (l .~ l')
    $ body
    where [x',r',p',l'] = takesV [3,9,3,3] vec


