{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Scratch where

import Op
import Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

finalout = outT $ switchT (chopChan0 $ invert [held]) [deckA, deckB]

secChop = constC [floor seconds]

invert l = logic' (logicPreop ?~ int 1) l

movieIndA = hold movieInd held
movieIndB = hold movieInd (invert [held])

movies = table $ transpose $ fromLists [moviesList]

votes = table $ BS.pack . show <$> fromLists votesList

deckA = movieFileIn' ((moviePlayMode ?~ int 1) . (movieIndex ?~ (frames !% int (60 * 60 * 3)))) $
  (cell ((casti $ floor $ chopChan0 movieIndA) !% int (length moviesList), int 0) movies)

deckB = movieFileIn' ((moviePlayMode ?~ int 1) . (movieIndex ?~ (frames !% int (60 * 60 * 3)))) $
  (cell ((casti $ floor $ chopChan0 movieIndB) !% int (length moviesList), int 0) movies)

noiseCount r s v = count' ((countThresh ?~ (float 0.5)) . (countReset ?~ r)) (math' opsadd [v])

-- (noiseChop & pars.noiseCTimeSlice ?~ (B True) & pars.noiseCTranslate._1 ?~ (float s) & pars.noiseCPeriod.~Just (float 0.3))

noiseTrigger r s v = count' ((countReset ?~ r) . (countThresh ?~ (float 4))) $ noiseCount r s v

mergedTriggers r = fix "triggers" $ fan' ((fanOp .~ (Just $ int 1)) . (fanOffNeg ?~ bool True)) $ mergeC $ zipWith (noiseTrigger r) [0, 2, 4] voteNums

trigfb = feedbackC (constC [float 0]) mergedTriggers (math' (opaddf 1) . (:[]))

voteResult = hold trigfb (math' (opaddf 1) [trigfb])

movieInd = constC . (:[]) $ castf $ cell (casti $ chopChan0 currentVote, pmax (int 0) $ casti $ chopChan0 voteResult) votes

held = logic' ((logicPreop ?~ (int 2)) . (logicConvert ?~ (int 3))) [movieInd]

currentVote = count' ((countLimType ?~ (int 1)) . (countLimMax ?~ (float (fromIntegral (length votesList) - 1)))) trigfb

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

server = tcpipD' ((tcpipMode ?~ (int 1)) . (tcpipCallbackFormat ?~ (int 2)) . (datVars .~ zipWith (\i v -> (BS.pack $ "vote" ++ show i, Resolve v)) [0..] voteNums)) (fileD "scripts/server.py")

peers = fix "myPeers" $ textD ""

closepeer = fix "closePeer" $ textD "args[0].close()"

website = fix "website" $ fileD "scripts/website.html"

voteNums = zipWith (\i c -> fix (BS.pack $ "voteNum" ++ show i) c) [0..] [constC [(float 0)], constC [(float 0)], constC [(float 0)]]

go = do ms <- run [server, website, closepeer] mempty
        ms <- run voteNums ms
        run [finalout] ms
