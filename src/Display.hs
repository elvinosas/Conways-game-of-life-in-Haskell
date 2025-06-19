module Display (display, idle, Rectangle (..)) where

import Control.Monad
import Data.IORef
import Game (State (..), process, render)
import Graphics.UI.GLUT
import Render (renderRect)
import Types

display :: IORef State -> DisplayCallback
display state = do
  clear [ColorBuffer]
  matrixMode $= Projection
  loadIdentity
  ortho 0 1024 768 0 0 100
  matrixMode $= Modelview 0

  s <- get state
  preservingMatrix $ render s

  swapBuffers

idle :: IORef State -> IORef Bool -> IdleCallback
idle state isRunning = do
  run <- get isRunning
  currentState <- readIORef state
  when run $ do
    newStateIO <- process currentState
    writeIORef state newStateIO
  postRedisplay Nothing
