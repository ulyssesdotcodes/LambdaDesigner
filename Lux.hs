{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Scratch where

import Op
import Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

finalout = outTop $ switchTop (chopChan0 $ invert held) [deckA, deckB]

secChop = constChop (floor seconds)

invert l = logic (logicPreop ?~ int 1) l

movieIndA = hold movieInd held
movieIndB = hold movieInd (invert held)

movies = table $ transpose $ fromLists [moviesList]

votes = table $ BS.pack . show <$> fromLists votesList

deckA = movieFileIn <&> pars.moviePlayMode .~ Just (int 1) <&> pars.movieIndex .~ Just (frames !% int (60 * 60 * 3)) $
  (cell ((floori $ chopChan0 movieIndA) !% int (length moviesList), int 0) movies)

deckB = movieFileIn <&> pars.moviePlayMode .~ Just (int 1) <&> pars.movieIndex .~ Just (frames !% int (60 * 60 * 3)) $
  (cell ((floori $ chopChan0 movieIndB) !% int (length moviesList), int 0) movies)

noiseCount s v r = (countReset
                  <&> (<&> pars.countThresh .~ Just (float 0.5)))
                  (opsadd [v]) r

-- (noiseChop & pars.noiseCTimeSlice .~ Just (B True) & pars.noiseCTranslate._1 .~ Just (float s) & pars.noiseCPeriod.~Just (float 0.3))

noiseTrigger r s v = flip countReset r <&> pars.countThresh .~ Just (float 4) $ noiseCount s v r

mergedTriggers r = fix "triggers" $ fan <&> pars.fanOp .~ (Just $ int 1) <&> pars.fanOffNeg .~ Just ptrue $ mergeChop $ zipWith (noiseTrigger r) [0, 2, 4] voteNums

trigfb = feedbackChop (constChop $ float 0) mergedTriggers (selectChop . opaddf 1)

voteResult = hold trigfb (opaddf 1 trigfb) & pars.logicPreop .~ Just (int 2) & pars.logicConvert .~ Just (int 3)

movieInd = constChop $  cellf (casti $ chopChan0 currentVote, pmax 0 $ casti $ chopChan0 voteResult) votes

held = logic movieInd & pars.logicPreop .~ Just (int 2) & pars.logicConvert .~ Just (int 3)

currentVote = count trigfb & pars.countLimType .~ Just (int 1) & pars.countLimMax .~ Just (float (fromIntegral (length votesList) - 1))

moviesList = map (\i -> BS.concat ["C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\Anna - Copy (", BS.pack $ show i, ").mp4"]) [1..34]
  ++
  ["C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\David.mp4"
  , "C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\Helen.mp4"
  ]

votesList = [ [19, 34, 35]
            , [0, 34, 35]
            , [20, 34, 35]
            , [10, 34, 35]
            , [15, 34, 35]
            ]

server = tcpipDat (fileDat "scripts/server.py") & pars.tcpipMode .~ Just (int 1) & pars.tcpipCallbackFormat .~ Just (int 2)

peers = fix "myPeers" $ textDat ""

closepeer = fix "closePeer" $ textDat "args[0].close()"

website = fix "website" $ fileDat "scripts/website.html"

voteNums = zipWith (\i c -> fix (BS.pack $ "voteNum" ++ show i) c) [0..] [constChop (float 0), constChop (float 0), constChop (float 0)]

go = do ms <- run [server, website, closepeer] mempty
        ms <- run voteNums ms
        run [finalout] ms
