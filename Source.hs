module Source where

import Op
import Lib

import Control.Lens

go
  = run [outT $ visual] mempty

visual =
  vidIn
  & blur (float 27)
