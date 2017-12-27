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
    ain = math' (mathMult ?~ float 0.3) [audioIn & audioSpectrum]
    atex = chopToT ain
           & transformT' ((transformScale._2 ?~ float 0.05) . (transformTranslate._2 ?~ float 0.5) . (topResolution .~ iv2 (128, 128)))
           & (\t -> feedbackT t (\t' -> compT 0 [t, t' & levelT' (levelOpacity ?~ float 0.98) & transformT' (transformTranslate . _2 ?~ float (-0.01))]) id)
    gsop = gridS' ((gridRows ?~ int 128) . (gridColumns ?~ int  128) . (gridPrimitive ?~ int 0) . (gridSurfType ?~ int 1))
    gchop = sopToC gsop
    gpos = replaceC [gchop, math' (opsadd) [selectC' (selectCNames ?~ str "tz") gchop, atex & topToC' ((topToChopRName ?~ str "r") . (topToChopDownloadType ?~ int 1) . (topToChopCrop ?~ int 4)) & shuffleC (int 2)]]
    g = geo' (geoMat ?~ gmat) $ outS $ chopToS' id gpos $ Just gsop
    centerCam t r = cam' ((camTranslate .~ t) . (camPivot .~ v3mult (float (-1)) t) . (camRotate .~ r))
    grender = render' (renderLight ?~ (light' (lightShadowType ?~ int 2))) g (centerCam (v3 (float 0) (float 0) (float 2.5)) (v3 (float 45) (float 0) (seconds !* float 30)))
    gmat = constM' (constMatMap ?~ (palette neon 0 (1, 128) & translate' id (v2 (float 0) seconds)))
  in
    do r <- newIORef mempty
       run r [outT $ grender]

fade' f l o t = feedbackT t (\t' -> l $ compT 0 [t, levelT' (levelOpacity ?~ o) t']) f
fade = fade' id id

data Palette = Palette [BS.ByteString]
palette (Palette colors) t (w, h) = ramp' ((topResolution .~ iv2 (w, h)) . (rampType ?~ int t)) . scriptD (scr "palette_mapper.py") . table . transpose
  $ fromLists [colors]
translate' f t = transformT' ((transformExtend ?~ int 3) . (transformTranslate .~ t) . f)

scr = (++) "scripts/"

neon = Palette ["A9336B", "5F2F88", "CB673D", "87BB38"]
fire = Palette ["f07f13", "800909", "f27d0c", "fdcf58"]
buddhist = Palette ["0000FF", "FFFF00", "FF0000", "FFFFFF", "FF9800"]
tealness = Palette ["#6cb6bd", "#71b8b9", "#7abbb3", "#81bead", "#8cc1a5"]
