{-# LANGUAGE OverloadedStrings #-}
module Visuals where

import Op
import Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

data VoteType = Movie | Effect deriving Eq
data VoteEffect = VoteEffect VoteType BS.ByteString BS.ByteString BS.ByteString deriving Eq


ain = math' (mathMult ?~ float 7) [audioIn]
atex = chopToT $ ain
aspect = audioSpectrum $ audioIn
aspecttex = chopToT $ aspect

volume = analyze (int 6) ain
volc = chopChan0 volume

cTSMod tf s = chopToS' ((chopToSResample ?~ bool True) . (chopToSopAttrScope ?~ str "P") . (sopIns .~ [s])) (tf (sopToC s))

amult t = math' ((mathAlign ?~ int 7) . (mathCombChops ?~ int 3)) [t, math' ((mathAddPost ?~ (float 1)) . (mathMult ?~ float 2)) [ain]]

acirc = cTSMod amult (circleS' ((circArc ?~ int 1) . (circType ?~ int 2)))

asphere = cTSMod amult $ sphere' (sphereType ?~ int 3)

mnoise s = noiseC' ((noiseCType ?~ int 2) .
                         (noiseCPeriod ?~ float s) .
                         (noiseCTranslate._2 ?~ seconds !* float 20) .
                         (chopTimeSlice ?~ bool True))
mnoisec s = chopChan0 $ mnoise s

main =
  run [outT $ palettemap neon volc $ fade $ adata] mempty

-------------------

sphereNoise = geo' id $ outS asphere

-- gens
adata = tdata (float 1) atex
shapes = frag "shapes.frag" [ ("i_size", xV4 $ float 0.2)
                            , ("i_width", xV4 volc)
                            , ("i_sides", xV4 $ floor $ scycle 3 10)
                            ] []

movingSquiggly = rendered $ geo' ((geoTranslate .~ (Just $ chopChan0 $ mnoise 5, Just $ chopChan0 $ mnoise 10, Just $ float  0)) .
            (geoScale.each ?~ float 0.3) .
            (geoMat ?~ constM (constColor .~ (Just $ osin $ seconds, Just $ osin $ (seconds !* float 2), Just $ osin $ (seconds !* chopChan0 volume)))))
       $ outS acirc

-- effects

fade t = feedbackT t (\t' -> compT 0 [t, levelT' (levelOpacity ?~ float 0.99) t']) id
brightness b = levelT' (levelBrightness ?~ b)
edgesc c t = compT 0 [edges t, levelT' (levelOpacity ?~ c) t]
littleplanet = frag "little_planet.frag" []
lumidots = frag "lumidots.frag" []
mirror t = compT 0 [flipT' ((flipx ?~ bool True) . (flipy ?~ bool True)) t, t]
mosaic t s top = frag "mosaic.frag" [("uTime", xV4 t), ("uScale", xV4 s)] [top]
noisedisplace d top = frag "noise_displace.frag" [("uTime", xV4 seconds), ("uDisplacement", xV4 d)] [top]
palettecycle p t = multops [crop' ((cropLeft ?~ seconds) . (cropRight ?~ seconds)) $ palette p, t]
palettemap p o t = frag "palette_map.frag" [("uOffset", xV4 o), ("uSamples", xV4 $ float 16)] [t, palette p]
repeatT r top = frag "repeat.frag" [("i_repeat", xV4 r)] [top]
rotate r = transformT' (transformRotate ?~ r)
scale s = transformT' (transformScale .~ s)
scale' s = transformT' (transformScale .~ (Just s, Just s))
strobe s top = frag "strobe.frag" [("uSpeed", xV4 s)] [top]
translate t = transformT' (transformTranslate .~ t)
translatex x = translate $ emptyV2 & _1 ?~ x
translatey y = translate $ emptyV2 & _2 ?~ y

-- combiners

addops = compT 0
fadeops f = switchT' (switchTBlend ?~ bool True) f
multops = compT 27
overops = compT 31
triggerops f tops = switchT (chopChan0 $
                             count' ((countThresh ?~ float 0.5) .
                                     (countLimMax ?~ float (fromIntegral $ length tops)) .
                                     (countLimType ?~ int 1)
                                    ) f
                            ) tops

-- palettes

neon = Palette ["A9336B", "5F2F88", "CB673D", "87BB38"]

------------------------

tres = (topResolution .~ (Just $ int 1920, Just $ int 1080)) . (pixelFormat ?~ int 3)
scr = (++) "scripts/Visuals/"
frag s = glslTP' tres (scr s)
rendered g = render' (renderLight ?~ light) g cam
tdata v t = frag "audio_data.frag" [("i_volume", xV4 v)] [t]
data Palette = Palette [BS.ByteString]
palette (Palette colors) = ramp' (topResolution .~ iv2 (128, 0)) . scriptD (scr "palette_mapper.py") . table . transpose
  $ fromLists [colors]


