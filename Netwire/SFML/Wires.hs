{-# LANGUAGE Arrows #-}

module Netwire.SFML.Wires where

import Control.Monad hiding (when)
import Control.Wire
import Data.Monoid (Monoid, mempty)
import Prelude hiding (id, (.))
import SFML.Window

import Netwire.SFML

-- | Remembers the size of the window.
windowSize :: (Monoid e, Monad m) => Wire e m Input (Maybe (Int, Int))
windowSize = hold Nothing (Just <$> resized)

-- | Remembers the position of the mouse relative to the window.
mousePosition :: (Monoid e, Monad m) => Wire e m Input (Maybe (Int, Int))
mousePosition = hold Nothing (Just <$> mouseMoved)

keyPressed :: (Monoid e, Monad m) => KeyCode -> Wire e m Input ()
keyPressed kc = whenEvent p
    where
    p (SFEvtKeyPressed kc' _ _ _ _) | kc == kc' = Just ()
    p _ = Nothing

mousePressed :: (Monoid e, Monad m) => MouseButton -> Wire e m Input ()
mousePressed b = whenEvent p
    where
    p (SFEvtMouseButtonPressed b' _ _) | b == b' = Just ()
    p _ = Nothing

closed :: (Monoid e, Monad m) => Wire e m Input ()
closed = whenEvent p
    where
    p SFEvtClosed = Just ()
    p _ = Nothing

resized :: (Monoid e, Monad m) => Wire e m Input (Int,Int)
resized = whenEvent p
    where
    p (SFEvtResized x y) = Just (x,y)
    p _ = Nothing

lostFocus :: (Monoid e, Monad m) => Wire e m Input ()
lostFocus = whenEvent p
    where
    p SFEvtLostFocus = Just ()
    p _ = Nothing

gainedFocus :: (Monoid e, Monad m) => Wire e m Input ()
gainedFocus = whenEvent p
    where
    p SFEvtGainedFocus = Just ()
    p _ = Nothing

textEntered :: (Monoid e, Monad m) => Wire e m Input String
textEntered = whenEvent p
    where
    p (SFEvtTextEntered s) = Just s
    p _ = Nothing

-- TODO: clearly document what all these bools mean
keyEvent :: (Monoid e, Monad m) => Wire e m Input (KeyCode, Bool, Bool, Bool, Bool)
keyEvent = whenEvent p
    where
    p (SFEvtKeyPressed c m1 m2 m3 m4) = Just (c, m1, m2, m3, m4)
    p _ = Nothing

keyReleased :: (Monoid e, Monad m) => Wire e m Input (KeyCode, Bool, Bool, Bool, Bool)
keyReleased = whenEvent p
    where
    p (SFEvtKeyReleased c m1 m2 m3 m4) = Just (c, m1, m2, m3, m4)
    p _ = Nothing

mouseWheelMoved :: (Monoid e, Monad m) => Wire e m Input (Int, Int, Int)
mouseWheelMoved = whenEvent p
    where
    p (SFEvtMouseWheelMoved d x y) = Just (d, x, y)
    p _ = Nothing

mouseButtonPressed :: (Monoid e, Monad m) => Wire e m Input (MouseButton, Int, Int)
mouseButtonPressed = whenEvent p
    where
    p (SFEvtMouseButtonPressed b x y) = Just (b, x, y)
    p _ = Nothing

mouseButtonReleased :: (Monoid e, Monad m) => Wire e m Input (MouseButton, Int, Int)
mouseButtonReleased = whenEvent p
    where
    p (SFEvtMouseButtonReleased b x y) = Just (b, x, y)
    p _ = Nothing

mouseMoved :: (Monoid e, Monad m) => Wire e m Input (Int, Int)
mouseMoved = whenEvent p
    where
    p (SFEvtMouseMoved x y) = Just (x, y)
    p _ = Nothing

mouseEntered :: (Monoid e, Monad m) => Wire e m Input ()
mouseEntered = whenEvent p
    where
    p SFEvtMouseEntered = Just ()
    p _ = Nothing

mouseLeft :: (Monoid e, Monad m) => Wire e m Input ()
mouseLeft = whenEvent p
    where
    p SFEvtMouseLeft = Just ()
    p _ = Nothing

joystickButtonPressed :: (Monoid e, Monad m) => Wire e m Input (Int, Int)
joystickButtonPressed = whenEvent p
    where
    p (SFEvtJoystickButtonPressed id bt) = Just (id, bt)
    p _ = Nothing

joystickButtonReleased :: (Monoid e, Monad m) => Wire e m Input (Int, Int)
joystickButtonReleased = whenEvent p
    where
    p (SFEvtJoystickButtonReleased id bt) = Just (id, bt)
    p _ = Nothing

joystickMoved :: (Monoid e, Monad m) => Wire e m Input (Int, JoystickAxis, Float)
joystickMoved = whenEvent p
    where
    p (SFEvtJoystickMoved id a p) = Just (id, a, p)
    p _ = Nothing

joystickConnected :: (Monoid e, Monad m) => Wire e m Input Int
joystickConnected = whenEvent p
    where
    p (SFEvtJoystickConnected id) = Just id
    p _ = Nothing

joystickDisconnected :: (Monoid e, Monad m) => Wire e m Input Int
joystickDisconnected = whenEvent p
    where
    p (SFEvtJoystickDisconnected id) = Just id
    p _ = Nothing


-- Helpers
whenEvent :: (Monoid e, Monad m) => (a -> Maybe b) -> Wire e m (Maybe a) b
whenEvent f = mkFix $ \_ x -> maybe (Left mempty) Right (x >>= f)

