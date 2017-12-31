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
    sgeo = instanceGeo' ((geoMat ?~ wireframeM)) poses (outS $ lineS)
    instances = casti (volc !* float 30) !+ int 2
    width = float 20
    poses = mergeC' (mergeCAlign ?~ int 7) [tx, ty, rz, sx]
    tx = waveC' (waveCNames ?~ str "tx") instances $ ((castf (sampleIndex !* casti width !* int 2)) !/ castf instances) !- width
    ty = ain & resampleC' ((resampleEnd ?~ instances) . (resampleRate ?~ instances)) False & renameC (str "ty")
    rz = waveC' (waveCNames ?~ str "rz") instances $ osin (castf sampleIndex) !* float 360
    sx = waveC' (waveCNames ?~ str "sx") instances $ castf sampleIndex !* float 0.1
    centerCam t r = cam' ((camTranslate .~ t) . (camPivot .~ v3mult (float (-1)) t) . (camRotate .~ r))
    grender = render sgeo (centerCam (v3 (float 0) (float 0) (float 50)) emptyV3)
    volume = analyze (int 6) ain
    volc = chan0f volume
    -- ain = math' (mathMult ?~ float 0.3) [audioIn & audioSpectrum]
    -- atex = chopToT ain
    --        & transformT' ((transformScale._2 ?~ float 0.05) . (transformTranslate._2 ?~ float 0.5) . (topResolution .~ iv2 (128, 128)))
    --        & (\t -> feedbackT t (\t' -> compT 0 [t, t' & levelT' (levelOpacity ?~ float 0.98) & transformT' (transformTranslate . _2 ?~ float (-0.01))]) id)
    -- gsop = gridS' ((gridRows ?~ int 128) . (gridColumns ?~ int  128) . (gridPrimitive ?~ int 0) . (gridSurfType ?~ int 1))
    -- gchop = sopToC gsop
    -- gpos = replaceC [gchop, math' (opsadd) [selectC' (selectCNames ?~ str "tz") gchop, atex & topToC' ((topToChopRName ?~ str "r") . (topToChopDownloadType ?~ int 1) . (topToChopCrop ?~ int 4)) & shuffleC (int 2)]]
    -- g = geo' (geoMat ?~ gmat) $ outS $ chopToS' id gpos $ Just gsop
    -- centerCam t r = cam' ((camTranslate .~ t) . (camPivot .~ v3mult (float (-1)) t) . (camRotate .~ r))
    -- grender = render' (renderLight ?~ (light' (lightShadowType ?~ int 2))) g (centerCam (mchanv3 "s1" (float 5)) (mchanv3 "s2" (float 360)))
    -- gmat = constM' (constMatMap ?~ (palette neon 0 (1, 128) & translate' id (v2 (float 0) seconds)))
    -- mchanv3 pre m = v3 (m !* mchan (pre ++ "a")) (m !* mchan (pre ++ "b")) (m !* mchan (pre ++ "c"))
  in
    do r <- newIORef mempty
       run r [outT $ grender & fade (float 0.95)]

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
