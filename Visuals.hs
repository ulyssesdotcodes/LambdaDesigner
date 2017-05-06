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

volume = analyze (int 6) ain

cTSMod tf s = chopToS' ((chopToSResample ?~ bool True) . (chopToSopAttrScope ?~ str "P") . (sopIns .~ [s])) (tf (sopToC s))

amult t = math' ((mathAlign ?~ int 7) . (mathCombChops ?~ int 3)) [t, math' ((mathAddPost ?~ (float 1)) . (mathMult ?~ float 2)) [ain]]

acirc = cTSMod amult (circleS' ((circArc ?~ int 1) . (circType ?~ int 2)))

asphere = cTSMod amult $ sphere' (sphereType ?~ int 3)

movingNoise s = noiseC' ((noiseCType ?~ int 2) .
                         (noiseCPeriod ?~ float s) .
                         (noiseCTranslate._2 ?~ seconds !* float 20) .
                         (chopTimeSlice ?~ bool True))

rendered = render' (renderLight ?~ light) geom cam

fbr = feedbackT rendered (\t -> compT 0 [rendered, levelT' (levelOpacity ?~ float 0) t]) id

go = run [outT $ fbr] mempty

-------------------

geom = sphereNoise

movingSquiggly = geo' ((geoTranslate .~ (Just $ chopChan0 $ movingNoise 5, Just $ chopChan0 $ movingNoise 10, Just $ float  0)) .
            (geoScale.each ?~ float 0.3) .
            (geoMat ?~ constM (constColor .~ (Just $ osin $ seconds, Just $ osin $ (seconds !* float 2), Just $ osin $ (seconds !* chopChan0 volume)))))
       $ outS acirc

sphereNoise = geo' id $ outS asphere

------------------------

scr = (++) "scripts/Visuals/"

tdata v t = glslT' ((glslTUniforms .~ [("i_volume", emptyV4 & _1 ?~ v)]) . (topResolution._1 .~ (Just $ int 300))) (scr "audio_data.frag") [t]
