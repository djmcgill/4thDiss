module Engine.Display where

import Engine.RigidBody
import Engine.Objects
import Engine.View
import Engine.World

import Control.Lens
import Control.Monad (forM_)
import Graphics.Rendering.OpenGL hiding (Sphere)
import qualified Graphics.Rendering.OpenGL as GL
import Numeric.LinearAlgebra hiding (scale)
import SFML.Graphics hiding (rotate, scale, Quads)

drawWorld :: RenderWindow -> World -> IO ()
drawWorld window world = do
  setWindowActive window True
  GL.clear [ColorBuffer, DepthBuffer]

  matrixMode $= Projection
  loadIdentity
  perspective 90 1 1 50
  lookAtView $ world^.worldView

  forM_ (world^.objects) $ \(Object basicObj _ body) -> preservingMatrix $ do
    let [x1,x2,x3] = toList $ body^.x
    translate $ Vector3 x1 x2 x3
    drawBasicObject basicObj
  display window

lookAtView :: WorldView -> IO ()
lookAtView view = do
  let [eyeX, eyeY, eyeZ] = toList . negate $ view^.eyeLocation
  translate $ Vector3 eyeX eyeY eyeZ
  rotate (negate (view^.eyeVerticalTilt))   $ Vector3 1 0 0
  rotate (negate (view^.eyeHorizontalTilt)) $ Vector3 0 1 0

drawBasicObject :: BasicObject -> IO ()
drawBasicObject (Sphere radius) = renderQuadric style $ GL.Sphere radius 10 10
  where style = QuadricStyle
                  Nothing
                  NoTextureCoordinates
                  Outside
                  FillStyle
drawBasicObject (Cuboid xlen ylen zlen) = preservingMatrix $ do
  scale (xlen/2) (ylen/2) (zlen/2)
  renderPrimitive Quads . mapM_ vertex $
    [ Vertex3 (-1) (-1) ((-1) :: Double)
    , Vertex3 (-1)   1  (-1)
    , Vertex3   1  1  (-1)
    , Vertex3   1  (-1) (-1)

    , Vertex3 (-1) (-1)   1
    , Vertex3 (-1)   1  1
    , Vertex3   1  1  1
    , Vertex3   1  (-1)   1

    , Vertex3 (-1) (-1) (-1)
    , Vertex3 (-1)   1  (-1)
    , Vertex3 (-1)   1  1
    , Vertex3 (-1) (-1)   1

    , Vertex3   1  (-1) (-1)
    , Vertex3   1  1  (-1)
    , Vertex3   1  1  1
    , Vertex3   1  (-1)   1

    , Vertex3 (-1) (-1)   1
    , Vertex3 (-1) (-1) (-1)
    , Vertex3   1  (-1) (-1)
    , Vertex3   1  (-1)   1

    , Vertex3 (-1)   1  1
    , Vertex3 (-1)   1  (-1)
    , Vertex3   1  1  (-1)
    , Vertex3   1  1  1 ]
