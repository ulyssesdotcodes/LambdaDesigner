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
    gsop = gridS' ((gridRows ?~ int 128) . (gridColumns ?~ int  128))
    gchop = sopToC gsop
    gpos = replaceC [gchop, math' (opsadd) [selectC' (selectCNames ?~ str "tz") gchop, atex & topToC' ((topToChopRName ?~ str "r") . (topToChopDownloadType ?~ int 1) . (topToChopCrop ?~ int 4)) & shuffleC (int 2)]]
    g = geo' id $ outS $ chopToS' id gpos $ Just gsop
    grender = render' (renderLight ?~ (light' (lightShadowType ?~ int 2))) g cam
  in
    do r <- newIORef mempty
       run r [outT $ grender & edges & levelT' (levelInvert ?~ float 1)]

fade' f l o t = feedbackT t (\t' -> l $ compT 0 [t, levelT' (levelOpacity ?~ o) t']) f
fade = fade' id id
