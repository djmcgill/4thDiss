module Netwire.SFML where

import Control.Monad (unless, when)
import Control.Wire hiding (when, unless, window)
import Data.Maybe (isJust, fromJust)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.IORef (newIORef, readIORef, writeIORef)
import Prelude hiding (id, (.))

import SFML.Graphics (RenderWindow, createRenderWindow)
import SFML.Window

type Input = Maybe SFEvent
data Output world = Output
    { world :: world
    , toDraw :: Bool
    , toExit :: Bool}

adaptSimple
    :: String
    -> (RenderWindow -> world -> IO ())
    -> Wire e IO Input (Output world)
    -> IO ()
adaptSimple str draw wire = simpleInit str >>= runGameWire 60 draw (return ()) wire

simpleInit :: String -> IO RenderWindow
simpleInit title = do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2
    createRenderWindow (VideoMode 640 480 32) title [SFDefaultStyle] ctxSettings

runGameWire
    :: Int                              -- ^ The desired FPS, set to 0 for unlimited
    -> (RenderWindow -> world -> IO ()) -- ^ An action to draw the world in the window
    -> IO ()                            -- ^ A finalise action that's run on close
    -> Wire e IO Input (Output world)   -- ^ The wire to run
    -> RenderWindow                     -- ^ The window to use
    -> IO ()
runGameWire fps draw finalise wire window = do
    -- window.PreserveOpenGLStates( true );
    lastEvent <- newIORef =<< getCurrentTime -- TODO: Maybe use SFML's timer instead?
    loop wire lastEvent
    destroy window
    finalise
    where
    spf = 1 / fromIntegral fps
    draw' = draw window
    loop w lastEvent = do
        now <- getCurrentTime
        dt  <- realToFrac . diffUTCTime now <$> readIORef lastEvent
        let processEvent mEvent | isJust mEvent || fps == 0 || dt > spf = do
                writeIORef lastEvent now
                (eOut, w') <- stepWire w dt mEvent
                either (error "wire inhibited") -- XXX: how to deal with exceptions?
                       -- TODO: continue computing the next step in parallel?
                       (\out -> unless (toExit out) (when (toDraw out) (draw' (world out)) >> loop w' lastEvent))
                       eOut
            processEvent _ = sfSleep (seconds 0.01) >> loop w lastEvent -- TODO: check that it's not minimised
        pollEvent window >>= processEvent