{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Scratch where

import Op
import Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

data VoteType = Movie | Effect deriving Eq
data VoteEffect = VoteEffect VoteType BS.ByteString BS.ByteString BS.ByteString deriving Eq

vtToBS Movie = "movie"
vtToBS Effect = "effect"
veToBS (VoteEffect (vtToBS -> ty) i1 i2 i3) = ty:[i1, i2, i3]

finalout = outT $  switchT (chopChan0 $ invert [held]) [deckA, deckB]

secChop = constC [floor seconds]

invert l = logic' (logicPreop ?~ int 1) l

movieIndA = hold movieInd held
movieIndB = hold movieInd (invert [held])

movies = table $ transpose $ fromLists [moviesList]

deckA = movieFileIn' ((moviePlayMode ?~ int 1) . (movieIndex ?~ (frames !% int (60 * 60 * 3)))) $
  (cell ((casti $ floor $ chopChan0 movieIndA) !% int (length moviesList), int 0) movies)

deckB = movieFileIn' ((moviePlayMode ?~ int 1) . (movieIndex ?~ (frames !% int (60 * 60 * 3)))) $
  (cell ((casti $ floor $ chopChan0 movieIndB) !% int (length moviesList), int 0) movies)

voteCount r v = count' ((countThresh ?~ (float 0.5)) . (countReset ?~ r) . (countResetCondition ?~ int 0)) v

-- (noiseChop & pars.noiseCTimeSlice ?~ (B True) & pars.noiseCTranslate._1 ?~ (float s) & pars.noiseCPeriod.~Just (float 0.3))
-- noiseTrigger r s v = count' ((countReset ?~ r) . (countThresh ?~ (float 4))) $ noiseCount r s v

maxVote = fan' ((fanOp .~ (Just $ int 1)) . (fanOffNeg ?~ bool False)) $
            math' ((mathCombChops ?~ (int 4)) . (mathInt ?~ (int 2)))
              [ mergeC $ voteCount (constC [voteEnabled]) <$> voteNums
              , math' (mathCombChops ?~ (int 7)) $ voteCount (constC [voteEnabled]) <$> voteNums
              ]

-- maxVote' = feedbackC (constC [float 0]) (voteEnabled) (math' (opaddf 1) . (:[]))

voteEnabled = ceil $ chopChanName "timer_fraction" voteTimer

-- voteResult = hold trigfb (math' (opaddf 1) [trigfb])


held = logic' ((logicPreop ?~ (int 2)) . (logicConvert ?~ (int 3))) [movieInd]

-- currentVote = count' ((countLimType ?~ (int 1)) . (countLimMax ?~ (float (fromIntegral (length votesList) - 1)))) maxVote

moviesList = map (\i -> BS.concat ["C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\Anna - Copy (", BS.pack $ show i, ").mp4"]) [1..34]
  ++
  ["C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\David.mp4"
  , "C:\\Users\\Ulysses Popple\\Development\\Lux-TD\\3 min\\Helen.mp4"
  ]

votesList = veToBS <$> [ VoteEffect Movie "0" "0" "0"
                       , VoteEffect Effect "bandw" "vhs" "bandw"
                       , VoteEffect Movie "19" "34" "35"
                       , VoteEffect Movie "0" "34" "35"
                       , VoteEffect Effect "vhs" "vhs" "bandw"
                       , VoteEffect Movie "20" "34" "35"
                       , VoteEffect Movie "10" "34" "35"
                       , VoteEffect Effect "vanish" "fade" "dim"
                       , VoteEffect Movie "15" "34" "35"
                       ]
votes = table $ fromLists votesList
currentVote = selectD' (selectDRI ?~ (casti $ (chopChanName "segment" voteTimer) !+ (chopChanName "running" voteTimer))) votes
prevVote = selectD' (selectDRI ?~ (casti $ (chopChanName "segment" voteTimer))) votes

movieVote = selectD' (selectDRExpr ?~ PyExpr "re.match('movie',me.inputCell.val) != None") prevVote
movieInd' = constC . (:[]) $ castf $ cell ((int 0), casti (chopChan0 maxVote) !+ int 1) movieVote
movieInd = feedbackC (constC [float 0]) (\m -> hold (mergeC' (mergeCDupes ?~ int 1) [movieInd', m]) (invert $ [constC [voteEnabled]])) id

effects = [ fix "bandw" $ N $ GLSLTOP (fileD "scripts/bandw.glsl") [] []
          , fix "vhs" $ N $ GLSLTOP (fileD "scripts/vhs.glsl") [("i_time", emptyV4 & _1 ?~ seconds)] []
          ]

effectVote = selectD' (selectDRExpr ?~ PyExpr "re.match('effect',me.inputCell.val) != None") prevVote
effectRunner t = datExec' ((datVars .~ [("base", Resolve finalout), ("voteResult", Resolve maxVote)]) . (deTableChange ?~ t)) effectVote

server = tcpipD' ((tcpipMode ?~ (int 1)) . (tcpipCallbackFormat ?~ (int 2)) . (datVars .~ [("website", Resolve website), ("timer", Resolve voteTimer)] ++ zipWith (\i v -> (BS.pack $ "vote" ++ show i, Resolve v)) [0..] voteNums))
            (fix "server" $ fileD' (datVars .~ [("timer", Resolve voteTimer)]) "scripts/server.py")

sendServer = datExec' (deTableChange ?~ "  mod.server.updateVotes(dat[0, 1].val, dat[0,2].val, dat[0,3].val)") currentVote
sendEnabled = chopExec' (ceValueChange ?~ "  mod.server.enableVotes(val)") $ constC [voteEnabled]

peers = fix "myPeers" $ textD ""

closepeer = fix "closePeer" $ textD "args[0].close()"

website = fileD "scripts/website.html"

voteNums = zipWith (\i c -> fix (BS.pack $ "voteNum" ++ show i) c) [0..] [constC [(float 0)], constC [(float 0)], constC [(float 0)]]

voteTimer = timerSeg' ((timerShowSeg ?~ bool True)) [ TimerSegment 5 8
                                                    , TimerSegment 5 8
                                                    , TimerSegment 5 8
                                                    , TimerSegment 5 8
                                                    , TimerSegment 5 8
                                                    , TimerSegment 5 8
                                                    ]

-- votesExec = (deTableChange ?~ "print(dat[0,0])") $ selectD' (selectDRI ?~ casti (chopChanName "segment" voteTimer)) votes


go = do ms <- run [server, closepeer, sendServer, sendEnabled] mempty
        eft <- BS.readFile "TD/scripts/effectChangeScript.py"
        ms <- run effects ms
        ms <- run [effectRunner eft] ms
        run [finalout] ms
