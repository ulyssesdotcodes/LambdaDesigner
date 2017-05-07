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
volc = chopChan0 volume

cTSMod tf s = chopToS' ((chopToSResample ?~ bool True) . (chopToSopAttrScope ?~ str "P") . (sopIns .~ [s])) (tf (sopToC s))

amult t = math' ((mathAlign ?~ int 7) . (mathCombChops ?~ int 3)) [t, math' ((mathAddPost ?~ (float 1)) . (mathMult ?~ float 2)) [ain]]

acirc = cTSMod amult (circleS' ((circArc ?~ int 1) . (circType ?~ int 2)))

asphere = cTSMod amult $ sphere' (sphereType ?~ int 3)

movingNoise s = noiseC' ((noiseCType ?~ int 2) .
                         (noiseCPeriod ?~ float s) .
                         (noiseCTranslate._2 ?~ seconds !* float 20) .
                         (chopTimeSlice ?~ bool True))

go = run [outT $ fade movingSquiggly] mempty

-------------------

sphereNoise = geo' id $ outS asphere

-- gens
adata = tdata (float 1) atex
shapes = frag "shapes.frag" [ ("i_size", xV4 $ float 0.2)
                            , ("i_width", xV4 volc)
                            , ("i_sides", xV4 $ floor $ scycle 3 10)
                            ] []

movingSquiggly = rendered $ geo' ((geoTranslate .~ (Just $ chopChan0 $ movingNoise 5, Just $ chopChan0 $ movingNoise 10, Just $ float  0)) .
            (geoScale.each ?~ float 0.3) .
            (geoMat ?~ constM (constColor .~ (Just $ osin $ seconds, Just $ osin $ (seconds !* float 2), Just $ osin $ (seconds !* chopChan0 volume)))))
       $ outS acirc

-- effects

fade t = feedbackT t (\t' -> compT 0 [t, levelT' (levelOpacity ?~ float 0.97) t']) id

------------------------

tres = topResolution .~ (Just $ int 1920, Just $ int 1080)
scr = (++) "scripts/Visuals/"
frag s = glslTP' tres (scr s)
rendered g = render' (renderLight ?~ light) g cam
tdata v t = frag "audio_data.frag" [("i_volume", xV4 v)] [t]


