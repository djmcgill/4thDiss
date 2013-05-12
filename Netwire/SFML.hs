module Netwire.SFML where
-- TODO: rename to Control.Wire.SFML?

import Control.Concurrent (forkIO)
import Control.Monad (unless, when, void)
import Control.Wire hiding (when, unless, window)
import Data.Maybe (isJust)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.IORef (newIORef, readIORef, writeIORef)
import Prelude hiding (id, (.))

import SFML.Graphics (RenderWindow, createRenderWindow)
import SFML.Window

type Input = Maybe SFEvent
data Output world = Output {
    world :: world,
    toDraw :: Bool,
    toExit :: Bool}

adaptSimple
    :: String
    -> (RenderWindow -> world -> IO ())
    -> Wire e IO Input (Output world)
    -> IO ()
adaptSimple str draw wire = simpleInit str >>= runGameWire 30 draw (return ()) wire

simpleInit :: String -> IO RenderWindow
simpleInit title = createRenderWindow (VideoMode 640 480 32) title [SFDefaultStyle] ctxSettings
    where ctxSettings = Just $ ContextSettings 24 8 0 1 2

runGameWire
    :: Int                              -- ^ The desired FPS, set to 0 for unlimited
    -> (RenderWindow -> world -> IO ()) -- ^ An action to draw the world in the window
    -> IO ()                            -- ^ A finalise action that's run on close
    -> Wire e IO Input (Output world)   -- ^ The wire to run
    -> RenderWindow                     -- ^ The window to use
    -> IO ()
runGameWire fps draw finalise wire window = do
    -- TODO: maybe "window.PreserveOpenGLStates( true );"?
    lastEvent <- getCurrentTime
    sfSleep (seconds 0.01)
    loop wire lastEvent
    destroy window
    finalise
    where
    spf = 1 / fromIntegral fps
    draw' = draw window
    loop w lastEvent = do
        now <- getCurrentTime
        let dt = realToFrac $ diffUTCTime now lastEvent
        let processEvent mEvent | isJust mEvent || fps == 0 || dt > spf = do
                (eOut, w') <- stepWire w dt mEvent
                either (error "wire inhibited")
                       (\out -> unless (toExit out) $ do
                                    when (toDraw out) . void . forkIO . draw' $ world out
                                    sfSleep (seconds (realToFrac spf))
                                    loop w' now)
                       eOut
            processEvent _ = sfSleep (seconds 0.01) >> loop w lastEvent
        pollEvent window >>= processEvent