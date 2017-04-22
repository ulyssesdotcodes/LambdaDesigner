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

held = logic secChop & pars.logicPreop .~ Just (int 2) & pars.logicConvert .~ Just (int 3)

invert l = logic l & pars.logicPreop .~ Just (int 1)

movieIndA = hold secChop held
movieIndB = hold secChop (invert held)

movies = table $ fromLists [["C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\Anna.mp4"
                            , "C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\David.mp4"
                            , "C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\Helen.mp4"
                            ]]

testTable = table $ matrix 3 3 (\(x, y) -> BS.pack $ show (x + y))


deckA = movieFileIn (cell (int 0, mod (floori $ chopChan0 movieIndA) (int 3)) movies)

deckB = movieFileIn (cell (int 0, mod (floori $ chopChan0 movieIndB) (int 3)) movies)

noiset = noiseTop
         & pars . noiseTMonochrome .~ Just (B False)
         & pars.noiseTResolution .~ (Just (int 1), Just (int 1))
         & pars.noiseTTranslate._3 .~ Just (float 0.2 !* seconds)

xynoise = noiseCHOP
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
  chopExec (noiseCHOP)
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

finalout = outTop $ feedbackTop (render circgeom cam) (levelTop <&> pars . levelOpacity .~ Just (float 1)) (compTop 31)
