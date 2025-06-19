{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.DeepSeq
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL

data Rectangle = Rectangle (Float, Float) (Float, Float) (Float, Float, Float)
  deriving (Show, Generic)

instance NFData Rectangle
