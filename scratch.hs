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
    tsneoutput = tableF "../tsneoutput.txt"
                 & scriptD "scripts/distance_dat.py" . (:[tsnepos])
                 & datToC' (datToChopFirstColumn ?~ int 2)
                 & expressionC [(PyExpr "max(0, sorted(map(float, me.inputs[0].chans()))[-4] - me.inputVal)")] . (:[])
                 & expressionC [(PyExpr "math.sqrt((max(map(float, me.inputs[0].chans())) - me.inputVal) % max(map(float, me.inputs[0].chans())))")] . (:[])
                 & expressionC [(PyExpr "me.inputVal / sum(map(float, me.inputs[0].chans()))")] . (:[])
    tsnedata = tableF "../tsnedata.txt" & datToC
    tsnepos = chopToD $ expressionC [PyExpr "me.inputVal * (max(me.inputs[0][me.chanIndex].vals) - min(me.inputs[0][me.chanIndex].vals)) + min(me.inputs[0][me.chanIndex].vals)"] [constC [float 0.8, float 0.8]]
    merged = expressionC [(PyExpr "me.inputs[1][me.chanIndex] * me.inputVal")] [tsnedata, tsneoutput]
             & math' (mathCombChans ?~ int 1) . (:[])
             & shuffleC (int 1)
  in
    do r <- newIORef mempty
       run r [outC $ merged]

fade' f l o t = feedbackT t (\t' -> l $ compT 0 [t, levelT' (levelOpacity ?~ o) t']) f
fade = fade' id id

data Palette = Palette [BS.ByteString]
palette (Palette colors) t (w, h) = ramp' ((topResolution .~ iv2 (w, h)) . (rampType ?~ int t)) . scriptD (scr "Visuals/palette_mapper.py") . (:[]) . table . transpose
  $ fromLists [colors]
translate' f t = transformT' ((transformExtend ?~ int 3) . (transformTranslate .~ t) . f)

scr = (++) "scripts/"


bw = Palette ["000000", "FFFFFF"]
neon = Palette ["A9336B", "5F2F88", "CB673D", "87BB38"]
fire = Palette ["f07f13", "800909", "f27d0c", "fdcf58"]
buddhist = Palette ["0000FF", "FFFF00", "FF0000", "FFFFFF", "FF9800"]
tealness = Palette ["#6cb6bd", "#71b8b9", "#7abbb3", "#81bead", "#8cc1a5"]
