import Bindings (display, keyboardMouse, reshape)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)
import Data.IORef
import Game
import Graphics.UI.GLUT
import Text.Printf (printf)

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Conways game of life"
  windowSize $= Size 1024 768
  reshapeCallback $= Just reshape

  state <- newIORef ready
  isRunning <- newIORef False
  frameCountRef <- newIORef 0
  delayRef <- newIORef (1000 `div` 60)

  keyboardMouseCallback $= Just (keyboardMouse isRunning state delayRef)

  let fpsReporter = forever $ do
        threadDelay (5 * 1000 * 1000)
        frames <- atomicModifyIORef' frameCountRef (\currentCount -> (0, currentCount))
        let fps = fromIntegral frames / 5.0 :: Double
        printf "FPS: %.2f\n" fps

  _ <- forkIO fpsReporter

  let timerCallback = do
        run <- readIORef isRunning
        when run $ do
          currentState <- readIORef state
          newState <- process currentState
          writeIORef state newState
          postRedisplay Nothing

        atomicModifyIORef' frameCountRef (\c -> (c + 1, ()))

        currentDelay <- readIORef delayRef
        addTimerCallback currentDelay timerCallback

  displayCallback $= display state

  addTimerCallback 0 timerCallback

  mainLoop
