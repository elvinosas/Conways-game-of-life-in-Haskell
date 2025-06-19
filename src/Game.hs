{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Game (process, State (..), ready, render, cellCount, generateCells, changeState) where

import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.DeepSeq
import Control.Monad (forM_)
import qualified Data.Map as Map
import Render (renderRect)
import Types

newtype State = State [Rectangle] deriving (Show)

cellSize :: Float
cellSize = 16

type GridCoord = (Int, Int)

getCoord :: Rectangle -> GridCoord
getCoord (Rectangle (x, y) _ _) = (round (x / cellSize), round (y / cellSize))

isAlive :: Rectangle -> Bool
isAlive (Rectangle _ _ (r, _, _)) = r /= 0

cellCount :: (Int, Int)
cellCount = (floor (1024 / cellSize), round (768 / cellSize))

generateCells :: [Rectangle]
generateCells = [Rectangle (cellSize * fromIntegral x, fromIntegral y * cellSize) (cellSize, cellSize) (0, 0, 0) | x <- [0 .. fst cellCount - 1], y <- [0 .. snd cellCount - 1]]

ready :: State
ready = State generateCells

process :: State -> IO State
process (State rects) = do
  let board = Map.fromList [(getCoord r, isAlive r) | r <- rects]

  newRects <-
    runConcurrently $
      traverse (\rect -> Concurrently . pure $!! calculateNewRect rect board) rects

  pure $ State newRects

--    process' :: [Rectangle] -> [Rectangle] -> [Rectangle]
--    process' [] _ = []
--    process' (x:xs) l = checkCell x l  : process' xs l
calculateNewRect :: Rectangle -> Map.Map GridCoord Bool -> Rectangle
calculateNewRect rect@(Rectangle pos s _) board =
  let (x, y) = getCoord rect
      currentState = Map.findWithDefault False (x, y) board

      -- Define the 8 neighbors' coordinates
      neighborCoords =
        [ (x + 1, y + 1),
          (x + 1, y),
          (x + 1, y - 1),
          (x, y - 1),
          (x - 1, y - 1),
          (x - 1, y),
          (x - 1, y + 1),
          (x, y + 1)
        ]

      -- Count live neighbors using the FAST board lookup
      liveNeighbors = length $ filter id [Map.findWithDefault False coord board | coord <- neighborCoords]

      -- Determine the new color based on the rules
      newColor = case (currentState, liveNeighbors) of
        (True, 2) -> (1, 1, 1) -- A live cell with 2 neighbors stays alive (white)
        (_, 3) -> (1, 1, 1) -- A cell with 3 neighbors becomes alive (white)
        _ -> (0, 0, 0) -- All other cells die or stay dead (black)
   in Rectangle pos s newColor

changeState :: State -> (Float, Float) -> State
changeState (State l) rect = State $ changeState' l rect
  where
    changeState' :: [Rectangle] -> (Float, Float) -> [Rectangle]
    changeState' [] _ = []
    changeState' (rect0@(Rectangle (x, y) s (c, _, _)) : xs) rect'@(x1, y1) = do
      let x3 = fromIntegral $ floor (x1 / 16) * 16 + 16
      let y3 = fromIntegral $ floor (y1 / 16) * 16 + 16
      if x == x3 && y == y3
        then
          if c == 1
            then Rectangle (x, y) s (0, 0, 0) : changeState' xs rect'
            else Rectangle (x, y) s (1, 1, 1) : changeState' xs rect'
        else rect0 : changeState' xs rect'

render :: State -> IO ()
render (State cells') = forM_ cells' renderRect
