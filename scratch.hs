{-# LANGUAGE OverloadedStrings #-}

import LambdaDesigner.Op
import LambdaDesigner.Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.IORef
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

go = do r <- newIORef mempty
        run r [outT $ transformT' (transformScale .~ (Just $ float 0.5 !^ float (-1), Just $ float 0.5 !^ float (-1))) $ rectangle (Just $ float 0.2, Just $ float 0.2)]
