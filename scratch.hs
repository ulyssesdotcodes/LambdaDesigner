{-# LANGUAGE OverloadedStrings #-}

import LambdaDesigner.Op
import LambdaDesigner.Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.IORef
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

go = do r <- newIORef mempty
        run r [outT $ textT $ caststr $ chopChanName "timer_fraction" $ timer' ((timerStart .~ True)) (int 500)]
