{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Scratch where

import Op
import Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.IORef
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

data VoteType = Movie | Effect deriving Eq
data VoteEffect = VoteEffect VoteType BS.ByteString BS.ByteString BS.ByteString deriving Eq

vtToBS Movie = "movie"
vtToBS Effect = "effect"
veToBS (VoteEffect (vtToBS -> ty) i1 i2 i3) = ty:[i1, i2, i3]

sidebyside = glslT' (topResolution .~ iv2 (1920 * 2, 1080)) "scripts/sidebyside.frag"
movieout = nullT $ switchT (chopChan0 $ invert [lastMovieInd]) [deckA, deckB]
finalout = outT $ sidebyside [movieout, voteview]

movieTimer = timer' id (int (60 * 60 * 3))

deck ind = movieFileIn' ((moviePlayMode ?~ int 1) .
                      (movieIndex ?~ casti (chopChanName "timer_frames" movieTimer)) .
                      (topResolution .~ iv2 (1920, 1080))) $
  (cell ((casti $ floor $ chopChan0 ind) !% int (length moviesList), int 0) movies)
deckA = hold movieInd lastMovieInd & deck
deckB = hold movieInd (invert [lastMovieInd]) & deck

movieVote = selectD' (selectDRExpr ?~ PyExpr "re.match('movie',me.inputCell.val) != None") prevVote
movieInd' = constC . (:[]) $ castf $ cell ((int 0), casti (chopChan0 maxVote) !+ int 1) movieVote
movieInd = feedbackC (constC [float 0]) (\m -> hold (mergeC' (mergeCDupes ?~ int 1) [movieInd', m]) (invert $ [constC [voteEnabled]])) id

voteEnabled = ceil $ chopChanName "timer_fraction" voteTimer


lastMovieInd = logic' ((logicPreop ?~ (int 2)) . (logicConvert ?~ (int 3))) [movieInd]

moviesList = map (\i -> BS.concat ["C:\\Users\\ulyssesp\\Development\\Lux-TD\\3 min\\Anna - Copy (", BS.pack $ show i, ").mp4"]) [1..34]
  ++
  [ "C:\\Users\\ulyssesp\\Development\\Lux-TD\\3 min\\David.mp4"
  , "C:\\Users\\ulyssesp\\Development\\Lux-TD\\3 min\\Helen.mp4"
  ]
movies = table $ transpose $ fromLists [moviesList]


-- Votes
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

voteCount r v = count' ((countThresh ?~ (float 0.5)) . (countReset ?~ r) . (countResetCondition ?~ int 0)) v

maxVote = fan' ((fanOp .~ (Just $ int 1)) . (fanOffNeg ?~ bool False)) $
            math' ((mathCombChops ?~ (int 4)) . (mathInt ?~ (int 2)))
              [ mergeC $ voteCount (constC [voteEnabled]) <$> voteNums
              , math' (mathCombChops ?~ (int 7)) $ voteCount (constC [voteEnabled]) <$> voteNums
              ]

voteview = let c x = (int 0, int x)
               mcell n =
                 transformT' (transformTranslate._2 ?~ float (fromIntegral (n - 2) * 0.33))
                 $ textT' (topResolution .~ iv2 (1920, 1080)) $ cell (c n) currentVote
  in compT 0 $ mcell <$> [1..3]

-- Effects

effects = [ fix "bandw" $ N $ GLSLTOP (fix "bandwfrag" $ fileD "scripts/Lux/bandw.glsl") [] Nothing (iv2 (1920, 1080)) [] Nothing
          , fix "vhs" $ N $ GLSLTOP (fix "vhsfrag" $ fileD "scripts/Lux/vhs.glsl") [("i_time", emptyV4 & _1 ?~ seconds)] Nothing (iv2 (1920, 1080)) [] Nothing
          ]

effectVote = selectD' (selectDRExpr ?~ PyExpr "re.match('effect',me.inputCell.val) != None") prevVote
effectRunner t = datExec' ((datVars .~ [("base", Resolve movieout), ("voteResult", Resolve maxVote)]) . (deTableChange ?~ t)) effectVote

--Server

server = fix "server"
  (fileD' (datVars .~ [ ("website", Resolve website)
                     , ("control", Resolve control)
                     , ("timer", Resolve voteTimer)
                     , ("movieTimer", Resolve movieTimer)
                     , ("base", Resolve movieout)
                     , ("outf", Resolve finalout)
                     ] ++ zipWith (\i v -> (BS.pack $ "vote" ++ show i, Resolve v)) [0..] voteNums) "scripts/Lux/server.py")
        & tcpipD' ((tcpipMode ?~ (int 1)) . (tcpipCallbackFormat ?~ (int 2)))

peers = fix "myPeers" $ textD ""
closepeer = fix "closePeer" $ textD "args[0].close()"
website = fileD "scripts/Lux/website.html"
control = fileD "scripts/Lux/control.html"

sendServer = datExec' (deTableChange ?~ "  mod.server.updateVotes(dat[0, 1].val, dat[0,2].val, dat[0,3].val)") currentVote

voteNums = zipWith (\i c -> fix (BS.pack $ "voteNum" ++ show i) c) [0..] [constC [(float 0)], constC [(float 0)], constC [(float 0)]]

voteTimer = timerSeg' ((timerShowSeg ?~ bool True) . (timerCallbacks ?~ fileD "scripts/Lux/Lux/timer_callbacks.py"))
  [ TimerSegment 0 8
  , TimerSegment 0 8
  , TimerSegment 0 8
  , TimerSegment 0 8
  , TimerSegment 0 8
  , TimerSegment 0 8
  ]

-- Helpers

invert l = logic' (logicPreop ?~ int 1) l
secChop = constC [floor seconds]


-- Run

go = do r <- newIORef mempty
        eft <- BS.readFile "TD/scripts/Lux/effectChangeScript.py"
        run2 r [server, closepeer, sendServer, effectRunner eft] effects
