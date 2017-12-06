{-# LANGUAGE OverloadedStrings #-}

import LambdaDesigner.Op
import LambdaDesigner.Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.IORef
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

go = do r <- newIORef mempty
        run r [outT $ textT' ((textFontSize ?~ (float 32)) . (textAlign .~ iv2 (0, 2))) (str "Testing 1\n2\n3\n")]
