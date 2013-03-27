{-# LANGUAGE Arrows #-}

module Netwire.SFML.Wires where

import Control.Monad hiding (when)
import Control.Wire
import Data.Maybe (fromJust, isJust)
import Data.Monoid (Monoid, mempty)
import Prelude hiding (id, (.))
import SFML.Window

import Netwire.SFML

--TODO: have a good think about possibly needed wires


-- | Remembers the size of the window.
-- | If it hasn't yet been resized, calls 'getWindowSize'.
windowSize :: Wire () IO Input (Int, Int)
windowSize = hold_ resized <|> getCurrentSize
    where
    getCurrentSize = proc (window, _) -> do
        (Vec2u w h) <- perform -< getWindowSize window
        returnA -< (fromIntegral w, fromIntegral h)

-- | Remembers the position of the mouse relative to the window.
--   If it hasn't yet been resized, calls 'getMousePosition'.
-- TODO: why not always just call it?
mousePosition :: Wire () IO Input (Int, Int)
mousePosition = hold_ mouseMoved <|> getCurrentMousePosition
    where
    getCurrentMousePosition = proc (window, _) -> do
        (Vec2i w h) <- perform -< getMousePosition (Just window)
        returnA -< (w, h)

keyPressed :: KeyCode -> Wire () IO Input ()
keyPressed kc = keyPressed kc = whenEvent p
    where
    p (SFEvtKeyPressed kc' _ _ _ _) | kc == kc' = Just ()
    p _ = Nothing

mousePressed :: MouseButton -> Wire () IO Input ()
mousePressed b = whenEvent p
    where
    p (SFEvtMouseButtonPressed b' x y) | b == b' = Just ()
    p _ = Nothing

-- also some kind of modifiers wire, check yampa-glut

simpleMousePosition :: Fractional a => Wire () IO Input (a, a)
simpleMousePosition = undefined -- scale mousePosition to between [-1,1]^2 using windowSize

closed :: Wire () IO Input ()
closed = whenEvent p
    where
    p SFEvtClosed = Just ()
    p _ = Nothing

resized :: Wire () IO Input (Int,Int)
resized = whenEvent p
    where
    p (SFEvtResized x y) = Just (x,y)
    p _ = Nothing

lostFocus :: Wire () IO Input ()
lostFocus = whenEvent p
    where
    p SFEvtLostFocus = Just ()
    p _ = Nothing

gainedFocus :: Wire () IO Input ()
gainedFocus = whenEvent p
    where
    p SFEvtGainedFocus = Just ()
    p _ = Nothing

textEntered :: Wire () IO Input String
textEntered = whenEvent p
    where
    p (SFEvtTextEntered s) = Just s
    p _ = Nothing

keyEvent :: Wire () IO Input (KeyCode, Bool, Bool, Bool, Bool)
keyEvent = whenEvent p
    where
    p (SFEvtKeyPressed c m1 m2 m3 m4) = Just (c, m1, m2, m3, m4)
    p _ = Nothing

keyReleased :: Wire () IO Input (KeyCode, Bool, Bool, Bool, Bool)
keyReleased = whenEvent p
    where
    p (SFEvtKeyReleased c m1 m2 m3 m4) = Just (c, m1, m2, m3, m4)
    p _ = Nothing

mouseWheelMoved :: Wire () IO Input (Int, Int, Int)
mouseWheelMoved = whenEvent p
    where
    p (SFEvtMouseWheelMoved d x y) = Just (d, x, y)
    p _ = Nothing

mouseButtonPressed :: Wire () IO Input (MouseButton, Int, Int)
mouseButtonPressed = whenEvent p
    where
    p (SFEvtMouseButtonPressed b x y) = Just (b, x, y)
    p _ = Nothing

mouseButtonReleased :: Wire () IO Input (MouseButton, Int, Int)
mouseButtonReleased = whenEvent p
    where
    p (SFEvtMouseButtonReleased b x y) = Just (b, x, y)
    p _ = Nothing

mouseMoved :: Wire () IO Input (Int, Int)
mouseMoved = whenEvent p
    where
    p (SFEvtMouseMoved x y) = Just (x, y)
    p _ = Nothing

mouseEntered :: Wire () IO Input ()
mouseEntered = whenEvent p
    where
    p SFEvtMouseEntered = Just ()
    p _ = Nothing

mouseLeft :: Wire () IO Input ()
mouseLeft = whenEvent p
    where
    p SFEvtMouseLeft = Just ()
    p _ = Nothing

joystickButtonPressed :: Wire () IO Input (Int, Int)
joystickButtonPressed = whenEvent p
    where
    p (SFEvtJoystickButtonPressed id bt) = Just (id, bt)
    p _ = Nothing

joystickButtonReleased :: Wire () IO Input (Int, Int)
joystickButtonReleased = whenEvent p
    where
    p (SFEvtJoystickButtonReleased id bt) = Just (id, bt)
    p _ = Nothing

joystickMoved :: Wire () IO Input (Int, JoystickAxis, Float)
joystickMoved = whenEvent p
    where
    p (SFEvtJoystickMoved id a p) = Just (id, a, p)
    p _ = Nothing

joystickConnected :: Wire () IO Input Int
joystickConnected = whenEvent p
    where
    p (SFEvtJoystickConnected id) = Just id
    p _ = Nothing

joystickDisconnected :: Wire () IO Input Int
joystickDisconnected = whenEvent p
    where
    p (SFEvtJoystickDisconnected id) = Just id
    p _ = Nothing


-- Helpers
whenEvent :: Monoid e => (a -> Maybe b) -> Wire e m (w, Maybe a) b
whenEvent f = mkFix $ \_ (_,x) -> maybe (Left mempty) Right (x >>= f)

