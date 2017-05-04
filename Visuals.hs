module Visuals where

import Op
import Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

data VoteType = Movie | Effect deriving Eq
data VoteEffect = VoteEffect VoteType BS.ByteString BS.ByteString BS.ByteString deriving Eq


ain = audioIn
atex = chopToT $ ain
aspect = audioSpectrum $ audioIn
aspecttex = chopToT $ aspect

cTSMod tf s = chopToS' ((chopToSResample ?~ bool True) . (chopToSopAttrScope ?~ str "P") . (sopIns .~ [s])) (tf (sopToC s))

amult t = math' ((mathAlign ?~ int 7) . (mathCombChops ?~ int 3)) [t, math' ((mathAddPost ?~ (float 1)) . (mathMult ?~ float 4)) [ain]]

acirc = cTSMod amult circleS
