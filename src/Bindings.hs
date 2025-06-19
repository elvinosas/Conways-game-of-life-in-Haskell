module Bindings (display, reshape, keyboardMouse, idle) where

import Data.IORef
import Display (display, idle)
import Game
import Graphics.UI.GLUT
import Types

reshape :: ReshapeCallback
reshape s = do
  viewport $= (Position 0 0, s)

keyboardMouse :: IORef Bool -> IORef State -> IORef Int -> KeyboardMouseCallback
keyboardMouse run stateRef delayRef key state modifiers position =
  case (key, state) of
    (Char ' ', Down) -> do
      atomicModifyIORef' run (\r -> (not r, ()))
      postRedisplay Nothing
    (Char 'a', Down) -> do
      atomicModifyIORef' delayRef (\d -> (d + 50, ()))
      postRedisplay Nothing
    (Char 'd', Down) -> do
      atomicModifyIORef' delayRef (\d -> (max 1 (d - 50), ()))
      postRedisplay Nothing
    (MouseButton LeftButton, Down) -> do
      let (Position x y) = position
      s <- get stateRef
      stateRef $= changeState s (fromIntegral x, fromIntegral y)
      postRedisplay Nothing
    (Char 'q', Down) ->
      leaveMainLoop
    _ -> return ()
