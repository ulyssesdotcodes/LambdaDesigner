{-# LANGUAGE OverloadedStrings #-}

import LambdaDesigner.Op
import LambdaDesigner.Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.IORef
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

go =
  let
    ain = math' (mathMult ?~ float 10) [audioIn]
    sgeo = instanceGeo' ((geoMat ?~ wireframeM)) poses (outS $ boxS' ((boxSScale._1 ?~ float 2) . (boxSScale._2 ?~ float 0.5)))
    instances = int 40
    poses = mergeC' (mergeCAlign ?~ int 7) [ty, tz]
    ty = waveC' (waveCNames ?~ str "ty") instances $ ((castf sampleIndex !* float (0.5)) !- float 3.5) !+ (seconds !* (float (-0.5)) !% float 1)
    tz = waveC' (waveCNames ?~ str "tz") instances $ (castf sampleIndex !* float (-1)) !+ (seconds !% float 2)
    centerCam t r = cam' ((camTranslate .~ t) . (camPivot .~ v3mult (float (-1)) t) . (camRotate .~ r))
    grender = render sgeo (centerCam (v3 (float 0) (float 0) (float 5)) emptyV3)
    volume = analyze (int 6) ain
    volc = chan0f volume
  in
    do r <- newIORef mempty
       run r [outT $ grender]

fade' f l o t = feedbackT t (\t' -> l $ compT 0 [t, levelT' (levelOpacity ?~ o) t']) f
fade = fade' id id

data Palette = Palette [BS.ByteString]
palette (Palette colors) t (w, h) = ramp' ((topResolution .~ iv2 (w, h)) . (rampType ?~ int t)) . scriptD (scr "Visuals/palette_mapper.py") . table . transpose
  $ fromLists [colors]
translate' f t = transformT' ((transformExtend ?~ int 3) . (transformTranslate .~ t) . f)

scr = (++) "scripts/"


bw = Palette ["000000", "FFFFFF"]
neon = Palette ["A9336B", "5F2F88", "CB673D", "87BB38"]
fire = Palette ["f07f13", "800909", "f27d0c", "fdcf58"]
buddhist = Palette ["0000FF", "FFFF00", "FF0000", "FFFFFF", "FF9800"]
tealness = Palette ["#6cb6bd", "#71b8b9", "#7abbb3", "#81bead", "#8cc1a5"]
