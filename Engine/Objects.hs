{-# LANGUAGE TemplateHaskell #-}

module Engine.Objects where

import Engine.RigidBody
import Netwire.SFML

import Control.Lens
import Control.Wire
import Numeric.LinearAlgebra
import Prelude hiding ((.), id)

data BasicObject
  = Sphere {_radius :: Double}
  | Cuboid {_xlen :: Double
           ,_ylen :: Double
           ,_zlen :: Double}
makeLenses ''BasicObject

data Object = Object
  { _basicObject :: BasicObject
  , _forces      :: Input -> (Acceleration, PostUpdateFun)
  , _body        :: RigidBody}
makeLenses ''Object

isSphere :: Object -> Bool
isSphere (Object (Sphere _) _ _) = True
isSphere _ = False

isCuboid :: Object -> Bool
isCuboid (Object (Cuboid _ _ _) _ _) = True
isCuboid _ = False

initialCube :: Object
initialCube = Object
                basicCuboid
                forceFunction
                (createUniformBody basicCuboid & x .~ startingPos)
  where
  forceFunction :: Input -> (Acceleration, PostUpdateFun)
  forceFunction _ = ( \_ -> (fromList [0,0,-10]
                    , fromList [0,0,0]), bounce)

  -- if z <= 1, reverse its vertical speed
  bounce :: RigidBody -> RigidBody
  bounce body
    | (body^.x) @> 2 <= 1 = p %~ mapVectorWithIndex flipVert $ body
    | otherwise       = body

  flipVert :: Int -> Double -> Double
  flipVert 2 = negate
  flipVert _ = id

  basicCuboid = Cuboid 1 1 1
  startingPos = fromList [0,0,10]

initialCube2 :: Object
initialCube2 = initialCube &
  (body.x.~ (fromList [0,0,20])).(body.p .~ (fromList [1,0,0]))

-- | Given an initial object, turn it into a Wire that holds its state
objectToWire :: Monad m => Object -> Wire e m Input Object
objectToWire (Object basic forces rigid) =
  Object basic forces <$> rigidObject rigid . arr forces

createUniformBody :: BasicObject -> RigidBody
createUniformBody (Cuboid x0 y0 z0) = updateStateVars $ RigidBody {
  _mass     = mass',
  _iBody    = iBody',
  _iBodyInv = inv iBody',

  _x    = zeroVector,
  _r    = zeroMatrix,
  _p    = zeroVector,
  _l    = zeroVector,

  _iInv  = undefined,
  _v     = undefined,
  _omega = undefined}
  where
  mass'  = x0*y0*z0
  iBody' = diag . scale (mass'/12) $ fromList [y0^2 + z0^2, x0^2 + z0^2, x0^2 + y0^2]
createUniformBody (Sphere r0) = updateStateVars $ RigidBody {
  _mass     = mass',
  _iBody    = iBody',
  _iBodyInv = inv iBody',

  _x    = zeroVector,
  _r    = zeroMatrix,
  _p    = zeroVector,
  _l    = zeroVector,

  _iInv  = undefined,
  _v     = undefined,
  _omega = undefined}
  where
  mass'  = (4/3)*pi*r0^3
  iBody' = diag . scale (r0^5 * pi) $ fromList [2/3, 8/15, 2/5]