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

finalout = outTop $ switchTop (chopChan0 $ invert held) [deckA, deckB]

secChop = constChop (floor seconds)

invert l = logic l & pars.logicPreop .~ Just (int 1)

movieIndA = hold movieInd held
movieIndB = hold movieInd (invert held)

movies = table $ transpose $ fromLists [moviesList]

votes = table $ BS.pack . show <$> fromLists votesList

deckA = movieFileIn <&> pars.moviePlayMode .~ Just (int 1) <&> pars.movieIndex .~ Just (frames !% int (60 * 60 * 3)) $
  (cell ((floori $ chopChan0 movieIndA) !% int (length moviesList), int 0) movies)

deckB = movieFileIn <&> pars.moviePlayMode .~ Just (int 1) <&> pars.movieIndex .~ Just (frames !% int (60 * 60 * 3)) $
  (cell ((floori $ chopChan0 movieIndB) !% int 3, int 0) movies)

noiseCount s = (countReset
               <&> (<&> pars.countThresh .~ Just (float 0.5)))
               (noiseChop
                & pars.noiseCTimeSlice .~ Just (B True)
                & pars.noiseCTranslate._1 .~ Just (float s)
                & pars.noiseCPeriod.~Just (float 0.3))

noiseTrigger r s = countReset (noiseCount s r) r & pars.countThresh .~ Just (float 4)

mergedTriggers r = fix "triggers" $ fan <&> pars.fanOp .~ (Just $ int 1) <&> pars.fanOffNeg .~ Just ptrue $ mergeChop $ map (noiseTrigger r) [0, 2, 4]

trigfb = feedbackChop (constChop $ float 0) mergedTriggers (opadd 1 . selectChop)

voteResult = hold trigfb trigfb & pars.logicPreop .~ Just (int 2) & pars.logicConvert .~ Just (int 3)

movieInd = constChop $  cellf (casti $ chopChan0 currentVote, pmax 0 $ casti $ chopChan0 voteResult) votes

held = logic movieInd & pars.logicPreop .~ Just (int 2) & pars.logicConvert .~ Just (int 3)

currentVote = count trigfb & pars.countLimType .~ Just (int 1) & pars.countLimMax .~ Just (float (fromIntegral (length votesList) - 1))

moviesList = map (\i -> BS.concat ["C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\Anna - Copy (", BS.pack $ show i, ").mp4"]) [1..34]
  ++
  ["C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\David.mp4"
  , "C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\Helen.mp4"
  ]

votesList = [ [19, 9, 10]
            , [0, 3, 32]
            , [20, 14, 23]
            , [10, 17, 29]
            , [15, 19, 6]
            ]


