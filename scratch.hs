{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Scratch where

import Op
import Tree
import Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

secChop = constChop (floor seconds)


invert l = logic l & pars.logicPreop .~ Just (int 1)

movieIndA = hold movieInd held
movieIndB = hold movieInd (invert held)

moviesList = ["C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\Anna.mp4"
             , "C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\David.mp4"
             , "C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\Helen.mp4"
             ]

movies = table $ fromLists [moviesList]

testTable = table $ matrix 3 3 (\(x, y) -> BS.pack $ show (x + y))


deckA = movieFileIn <&> pars.moviePlayMode .~ Just (int 1) <&> pars.movieIndex .~ Just (frames !% int (60 * 60 * 3)) $
  (cell (int 0, (floori $ chopChan0 movieIndA) !% int (length moviesList)) movies)

deckB = movieFileIn <&> pars.moviePlayMode .~ Just (int 1) <&> pars.movieIndex .~ Just (frames !% int (60 * 60 * 3)) $
  (cell (int 0, (floori $ chopChan0 movieIndB) !% int 3) movies)

noiset = noiseTop
         & pars . noiseTMonochrome .~ Just (B False)
         & pars.noiseTResolution .~ (Just (int 1), Just (int 1))
         & pars.noiseTTranslate._3 .~ Just (float 0.2 !* seconds)

xynoise = noiseChop
          & pars . noiseCChannel .~ Just (S "t[xy]")
          & pars . noiseCTimeSlice .~ Just (B True)
          & pars . noiseCPeriod .~ Just (float 20)

circsop = circleSop
          & pars . circType .~ Just (int 2)
          & pars . circArc .~ Just (int 1)

sampleColor i n = Just (sampleTop i (0,0) n)

color = constantMat
        & pars . constColor .~ (sampleColor 0 colorRamp, sampleColor 1 colorRamp, sampleColor 2 colorRamp)
        & pars.constAlpha.~ Just (float 0.3)

colorRamp = ramp [(0, 1.0, 0.0, 0.0, 1.0), (0.5, 0.0, 0.0, 1.0, 1.0), (0.5, 0.0, 0.0, 1.0, 1.0)]
            & pars . rampType .~ Just (int 1)
            & pars . rampResolution .~ (Just (int 200), Just (int 1))
            & pars.rampPhase .~ Just (seconds !* (float 0.4))

limitVal =
  "  if val > 3: \n\
  \     op('movie').par.value0 = 2 \n\
  \  return "

testchopexec =
  chopExec (noiseChop)
  & pars . ceValueChange .~ Just limitVal
  & pars . ceOffToOn .~ Just limitVal

circgeom = geo
           <&> pars . geoTranslate._1 .~ Just (chopChanName "tx" xynoise !* float 2.5)
           <&> pars . geoTranslate._2 .~ Just (chopChanName "ty" xynoise)
           <&> pars . geoMat .~ Just (treePar color)
           <&> pars . geoUniformScale .~ Just (float 0.4)
           $ outSop
           $ (noiseSop <&> pars . noiseSTranslate . _3 .~ Just (seconds !* (float 0.3))) $
              chopToSop circsop (sopToChop circsop)
              & pars.chopToSopAttrScope .~ Just (S "N")

finalout = outTop $ feedbackTop (render circgeom cam) (\a -> compTop 31 [(levelTop <&> pars . levelOpacity .~ Just (float 1) $ a), render circgeom cam])

noiseCount s = (countReset
               <&> (<&> pars.countThresh .~ Just (float 0.5)))
               (noiseChop & pars.noiseCTimeSlice .~ Just (B True) & pars.noiseCTranslate._1 .~ Just (float s))

noiseTrigger r s = countReset (noiseCount s r) r & pars.countThresh .~ Just (float 4)

mergedTriggers r = fix "triggers" $ fan <&> pars.fanOp .~ (Just $ int 1) <&> pars.fanOffNeg .~ Just ptrue $ mergeChop $ map (noiseTrigger r) [0, 2, 4]

trigfb = feedbackChop (constChop $ float 0) mergedTriggers (opadd 1 . selectChop)

movieInd = hold trigfb trigfb & pars.logicPreop .~ Just (int 2) & pars.logicConvert .~ Just (int 3)

held = logic movieInd & pars.logicPreop .~ Just (int 2) & pars.logicConvert .~ Just (int 3)


