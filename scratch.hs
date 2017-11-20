{-# LANGUAGE OverloadedStrings #-}

import LambdaDesigner.Op
import LambdaDesigner.Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.IORef
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

go = do r <- newIORef mempty
        run r [outT $ fade' id id (float 0.9) $ rectangle (Just $ float 0.5, Just $ float 0.5)]

fade' f l o t = feedbackT t (\t' -> l $ compT 0 [t, levelT' (levelOpacity ?~ o) t']) f
