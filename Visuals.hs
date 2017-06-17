{-# LANGUAGE OverloadedStrings #-}

module Visuals where

import Op
import Lib

import Prelude hiding (floor, mod, lines)

import Control.Lens
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

data VoteType = Movie | Effect deriving Eq
data VoteEffect = VoteEffect VoteType BS.ByteString BS.ByteString BS.ByteString deriving Eq


ain' m = math' (mathMult ?~ float m) [audioIn]
ain = ain' 1
atex = chopToT $ ain
aspect = audioSpectrum $ audioIn
aspecttex = chopToT $ aspect

volume = analyze (int 6) ain
volc = chopChan0 volume
lowv' = analyze (int 6) . lowPass . ain'
lowv = lowv' 4
lowvc' = chopChan0 . lowv'
lowvc = lowvc' 4
highv = analyze (int 6) $ highPass ain
highvc = chopChan0 highv
bandv b = analyze (int 6) $ bandPass b ain
bandvc = chopChan0 . bandv

cTSMod tf s = chopToS' ((chopToSResample ?~ bool True) . (chopToSopAttrScope ?~ str "P")) (tf (sopToC s)) (Just s)

amult t = math' ((mathAlign ?~ int 7) . (mathCombChops ?~ int 3)) [t, math' ((mathAddPost ?~ (float 1)) . (mathMult ?~ float 2)) [ain]]

acirc = cTSMod amult (circleS' ((circArc ?~ int 1) . (circType ?~ int 2)))

asphere = cTSMod amult $ sphere' (sphereType ?~ int 3)

mnoise s = noiseC' ((noiseCType ?~ int 2) .
                         (noiseCPeriod ?~ float s) .
                         (noiseCTranslate._2 ?~ seconds !* float 20) .
                         (chopTimeSlice ?~ bool True))
mnoisec s = chopChan0 $ mnoise s

launchmapping = strobe (float 10 !* mchan "s1c")
  $ scalexy (float 0.2 !* mchan "s1b")
  $ fade (mchan "s1")
  $ foldr (.) id (zipWith ($) (reverse effects) (reverse [1..(length effects)]))
  $ switchT (float (-1) !+ (chopChan0 $ hold buttons buttons))
          [adata (mchan "s1a" !* float 2), shapes (float 3 !+ scycle 1 3) (volc !* mchan "s2a") (mchan "s2b")]

effects = [ \n -> palettecycle' (passmchan n) neon
          , \n -> translatex' (passmchan n) (mchan ("s" ++ show n) !* seconds)
          , \n -> paletterepeatT' (passmchan n) neon (float 20 !* mchan ("s" ++ show n))
          , \n -> mirror' (passmchan n)
          , \n -> mosaic' (passmchan n) (seconds !* float 20) (float 60)
          , \n -> blur' (passmchan n) (float 56 !* mchan ("s" ++ show n))
          ]
passmchan m = topPasses ?~ casti (mchan $ "b" ++ show m)

buttons = math' (mathCombChops ?~ int 1) $ mheld <$> [1..4]
mheld n = constC [float (fromIntegral n) !* (mchan $ "b" ++ show (n + 8))]

stresstest = fadeops (float 0.5) [
       noisedisplace (float 10) $ mosaic (seconds !* float 20) (float 100) $
       fade (float 0.96) $ blur (float 128) $ palettecycle neon $
       flocking (float 0.5, float 1) (float 10 !* volc),
       blur (float 27) $
       fade (float 0.99) $ flocking (float 0.4, float 1) (float 10 !* volc)]

-------------------

sphereNoise = geo' id $ outS asphere

-- Gens
adata m = tdata m atex
flocking (c, s) sp = tox "toxes/Visuals/flockingGpu.tox" [ ("Cohesion", ResolveP c)
                                                            , ("Separation", ResolveP s)
                                                            , ("Alignment", ResolveP c)
                                                            , ("Speed", ResolveP sp)
                                                            ] (Nothing :: Maybe (Tree TOP))
lines w s = frag "lines.frag" [("i_width", xV4 w), ("i_spacing", xV4 s)] []
metaballs mat = let wrapJust n x = Just $ chopChan0 x !* float n
                    mball n r tx ty = metaball' ((metaballRadius .~ (wrapJust n r, wrapJust n r, wrapJust n r)) .
                                                (metaballCenter .~ (Just tx, Just ty, Nothing)))
                    lagmod = lag (float 0) (float 0.2)
                    noiset m = noiseC' ((chopTimeSlice ?~ bool True) .
                                        (noiseCTranslate._1 ?~ seconds !* float 0.3) .
                                        (noiseCTranslate._3 ?~ float (m * 3)) .
                                        (noiseCAmplitude ?~ (float (m + 1)) !* volc) .
                                        (noiseCChannels ?~ str "chan[1-3]"))
                    noisex = noiset 1
                    noisey = noiset 0
                in rendered . geo' (geoMat ?~ mat) .
                   outS $ mergeS [ mball 1 (lagmod lowv) (chopChan 0 noisex !+ float 0.2)
                                   (chopChan0 noisey)
                                 , mball 9 (lagmod highv)
                                   (chopChan 1 noisex !+ float (-0.2))
                                   (chopChan 1 noisey !+ float 0.7)
                                 , mball 4 (lagmod $ bandv (float 0.5))
                                   (chopChan 2 noisex !+ float (-0.2))
                                   (chopChan 2 noisey !+ float (-0.7))
                                 ]
movingSquiggly = rendered $ geo' ((geoTranslate .~ (Just $ chopChan0 $ mnoise 5, Just $ chopChan0 $ mnoise 10, Just $ float  0)) .
            (geoScale.each ?~ float 0.3) .
            (geoMat ?~ constM' (constColor .~ (Just $ osin $ seconds, Just $ osin $ (seconds !* float 2), Just $ osin $ (seconds !* chopChan0 volume)))))
       $ outS acirc
particlemover v a p s = tox "toxes/Visuals/particlemover.tox" [ ("Palette", ResolveP $ palette p)
                                                            , ("Vmult", ResolveP v)
                                                            , ("Emitalpha", ResolveP a)
                                                            , ("Shape", ResolveP s)
                                                            ] (Nothing :: Maybe (Tree TOP))

shapes sides w s = frag "shapes.frag" [ ("i_size", xV4 s)
                                      , ("i_width", xV4 w)
                                      , ("i_sides", xV4 sides)
                                      ] []

sineT x s a = frag "sine.frag" [("i_time", xV4 $ x), ("i_scale", xV4 $ s), ("i_amplitude", xV4 $ a)] []
stringtheory t a = frag "string_theory.frag" [ ("i_time", xV4 t)
                                             , ("i_angle", xV4 a)
                                             , ("i_angle_delta", xV4 $ float 0.2)
                                             , ("i_xoff", xV4 $ float 0)
                                             ] []
-- vidIn

-- Effects

fade' f o t = feedbackT t (\t' -> compT 0 [t, levelT' (levelOpacity ?~ o) t']) f
fade = fade' id
brightness' f b = levelT' (levelBrightness ?~ b)
brightness = brightness' id
--blur
paletterepeatT' f p r top = frag' f "color_repeat.frag" [("i_repeat", xV4 r)] [top, palette p]
paletterepeatT = paletterepeatT' id
edgesc' f c t = compT 0 [edges' f t, levelT' (levelOpacity ?~ c) t]
edgesc = edgesc' id
flowermod s = frag' id "flower_mod.frag" [("uSeconds", xV4 s)] . (:[])
hue h = hsvT' (hsvAdjHueOffset ?~ h)
littleplanet' f = frag' f "little_planet.frag" [] . (:[])
littleplanet = littleplanet' id
lumidots' f = frag' f "lumidots.frag" [] . (:[])
lumidots = lumidots' id
mirror' f t = compT' f 0 [flipT' ((flipx ?~ bool True) . (flipy ?~ bool True)) t, t]
mirror = mirror' id
mosaic' f t s top = frag' f "mosaic.frag" [("uTime", xV4 t), ("uScale", xV4 s)] [top]
mosaic = mosaic' id
noisedisplace' f d top = frag' f "noise_displace.frag" [("uTime", xV4 seconds), ("uDisplacement", xV4 d)] [top]
noisedisplace = noisedisplace' id
palettecycle' f p t = compT' f 27 [crop' ((cropLeft ?~ seconds) . (cropRight ?~ seconds)) $ palette p, t]
palettecycle = palettecycle' id
palettemap' f p o t = frag' f "palette_map.frag" [("uOffset", xV4 o), ("uSamples", xV4 $ float 16)] [t, palette p]
palettemap = palettemap' id
repeatT' f r top = frag' f "repeat.frag" [("i_repeat", xV4 r)] [top]
repeatT = repeatT' id
rotate' f r = transformT' (transformRotate ?~ r)
rotate = rotate' id
sat s = hsvT' (hsvAdjSatMult ?~ s)
scale' f s = transformT' (transformScale .~ ((!+ (float 1)) <$> fst s, (!+ (float 1)) <$> snd s))
scale = scale' id
scalexy' f s = scale' f (Just s, Just s)
scalexy = scalexy' id
strobe' f s top = frag' f "strobe.frag" [("uSpeed", xV4 s), ("uTime", xV4 seconds)] [top]
strobe = strobe' id
translate' f t = transformT' ((transformExtend ?~ int 3) . (transformTranslate .~ t) . f)
translate = translate' id
translatex' f x = translate' f $ emptyV2 & _1 ?~ x
translatex = translatex' id
translatey' f y = translate' f $ emptyV2 & _2 ?~ y
translatey = translatey' id
val v = hsvT' (hsvAdjValMult ?~ v)

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
fire = Palette ["f07f13", "800909", "f27d0c", "fdcf58"]
buddhist = Palette ["0000FF", "FFFF00", "FF0000", "FFFFFF", "FF9800"]
tealness = Palette ["#6cb6bd", "#71b8b9", "#7abbb3", "#81bead", "#8cc1a5"]

------------------------

tres = (topResolution .~ (Just $ int 1920, Just $ int 1080)) . (pixelFormat ?~ int 3)
scr = (++) "scripts/Visuals/"
frag = frag' id
frag' f s = glslTP' (tres . f) (scr s)
rendered g = render' (renderLight ?~ light) g cam
tdata v t = frag "audio_data.frag" [("i_volume", xV4 v)] [t]
data Palette = Palette [BS.ByteString]
palette (Palette colors) = ramp' (topResolution .~ iv2 (128, 0)) . scriptD (scr "palette_mapper.py") . table . transpose
  $ fromLists [colors]


