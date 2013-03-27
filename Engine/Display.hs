module Engine.Display where

import Engine.RigidBody
import Engine.World

import Control.Lens
import Control.Monad (forM_)
import Graphics.Rendering.OpenGL hiding (Sphere)
import qualified Graphics.Rendering.OpenGL as GL
import Numeric.LinearAlgebra hiding (scale)
import SFML.Graphics hiding (scale, Quads)


drawWorld :: RenderWindow -> World -> IO ()
drawWorld window world = do
    setWindowActive window True
    GL.clear [ColorBuffer, DepthBuffer]

    matrixMode $= Projection
    loadIdentity
    perspective 90 1 1 50 -- probably don't need to call this every frame

    lookAtView (world^.worldView)

    forM_ (world^.objects) $ \(Object basicObj _ body) -> preservingMatrix $ do
        let [x1,x2,x3] = toList (body^.x)
        translate (Vector3 x1 x2 x3)
        drawBasicObject basicObj
        print (x1,x2,x3)
    display window

lookAtView :: WorldView -> IO ()
lookAtView view =
    lookAt (Vertex3 eyeX eyeY eyeZ)
           (Vertex3 centreX centreY centreZ)
           (Vector3 upX upY upZ)
    where
    [eyeX, eyeY, eyeZ] = toList (view^.eyeLocation)
    [centreX, centreY, centreZ] = toList (view^.eyeLocation + view^.eyeForward)
    [upX, upY, upZ] = toList (view^.eyeUp)

drawBasicObject :: BasicObject -> IO ()
drawBasicObject (Sphere radius) = renderQuadric style prim
    where
    style = QuadricStyle Nothing NoTextureCoordinates Outside LineStyle -- TODO: solid sphere
    prim = GL.Sphere radius 10 10
drawBasicObject (Cuboid xlen ylen zlen) = preservingMatrix $ do
    scale (xlen/2) (ylen/2) (zlen/2)
    renderPrimitive Quads . mapM_ vertex $
        [ Vertex3 (-1) (-1) ((-1) :: Double)
        , Vertex3 (-1)   1  (-1)
        , Vertex3   1    1  (-1)
        , Vertex3   1  (-1) (-1)

        , Vertex3 (-1) (-1)   1
        , Vertex3 (-1)   1    1
        , Vertex3   1    1    1
        , Vertex3   1  (-1)   1

        , Vertex3 (-1) (-1) (-1)
        , Vertex3 (-1)   1  (-1)
        , Vertex3 (-1)   1    1
        , Vertex3 (-1) (-1)   1

        , Vertex3   1  (-1) (-1)
        , Vertex3   1    1  (-1)
        , Vertex3   1    1    1
        , Vertex3   1  (-1)   1

        , Vertex3 (-1) (-1)   1
        , Vertex3 (-1) (-1) (-1)
        , Vertex3   1  (-1) (-1)
        , Vertex3   1  (-1)   1

        , Vertex3 (-1)   1    1
        , Vertex3 (-1)   1  (-1)
        , Vertex3   1    1  (-1)
        , Vertex3   1    1    1 ]