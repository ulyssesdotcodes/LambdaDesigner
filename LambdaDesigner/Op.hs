{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaDesigner.Op where

import Prelude hiding (sin)

import LambdaDesigner.ParsedOps

import Control.Lens
import Data.Matrix
import Data.Maybe
import Data.Monoid

import Data.ByteString.Char8 as BS
import Data.List as L
import qualified Data.Bool as DB

data Channel

float :: Float -> Tree Float
float = PyExpr . pack . show

int :: Int -> Tree Int
int = PyExpr . pack . show

bool :: Bool -> Tree Bool
bool = PyExpr . DB.bool "0" "1"

str :: String -> Tree ByteString
str = PyExpr . pack . show

bstr :: String -> Tree ByteString
bstr = PyExpr . pack

casti :: (Integral i) => Tree f -> Tree i
casti = Mod (\fl -> BS.concat ["int(", fl, ")"])

castf :: (Floating f) => Tree i -> Tree f
castf = Mod (\fl -> BS.concat ["float(", fl, ")"])

castb :: Tree a -> Tree b
castb = Mod (\fl -> BS.concat ["bool(", fl, ")"])

caststr :: (Show a) => Tree a -> Tree ByteString
caststr = Mod (\s -> BS.concat ["str(", s, ")"])

(!+) :: (Show a) => Tree a -> Tree a -> Tree a
(!+) = Mod2 (\a b -> BS.concat ["(", a, "+", b, ")"])

(!-) :: (Show a) => Tree a -> Tree a -> Tree a
(!-) = Mod2 (\a b -> BS.concat ["(", a, "-", b, ")"])

(!*) :: (Show a) => Tree a -> Tree a -> Tree a
(!*) = Mod2 (\a b -> BS.concat ["(", a, "*", b, ")"])

(!/) :: (Show a) => Tree a -> Tree a -> Tree a
(!/) = Mod2 (\a b -> BS.concat ["(", a, "/", b, ")"])

(!^) :: (Show a) => Tree a -> Tree a -> Tree a
(!^) = Mod2 (\a b -> BS.concat ["(", a, "**", b, ")"])

(!%) :: (Show a) => Tree a -> Tree a -> Tree a
(!%) = Mod2 (\a b -> BS.concat ["(", a, "%", b, ")"])

(!==) :: Tree a -> Tree a -> Tree Bool
(!==) = Mod2 (\a b -> BS.concat ["(", a, "==", b, ")"])

(!>) :: Tree a -> Tree a -> Tree Bool
(!>) = Mod2 (\a b -> BS.concat ["(", a, ">", b, ")"])

(!>=) :: Tree a -> Tree a -> Tree Bool
(!>=) = Mod2 (\a b -> BS.concat ["(", a, ">=", b, ")"])

(!<) :: Tree a -> Tree a -> Tree Bool
(!<) = Mod2 (\a b -> BS.concat ["(", a, "<", b, ")"])

(!<=) :: Tree a -> Tree a -> Tree Bool
(!<=) = Mod2 (\a b -> BS.concat ["(", a, "<=", b, ")"])

(!||) :: Tree Bool -> Tree Bool -> Tree Bool
(!||) = Mod2 (\a b -> BS.concat ["(", a, ") or (", b, ")"])

(!&&) :: Tree Bool -> Tree Bool -> Tree Bool
(!&&) = Mod2 (\a b -> BS.concat ["(", a, ") and (", b, ")"])


ternary :: Tree Bool -> Tree a -> Tree a -> Tree a
ternary = Mod3 (\a b c -> BS.concat ["(", b, " if ", a, " else ", c, ")"])

seconds :: Tree Float
seconds = PyExpr "absTime.seconds"

frames :: Tree Int
frames = PyExpr "absTime.frame"

sampleIndex :: Tree Int
sampleIndex = PyExpr "me.sampleIndex"

chanIndex :: Tree Int
chanIndex = PyExpr "me.chanIndex"

opInput :: (Op a) => Tree Int -> Tree a
opInput = Mod (\i -> BS.concat ["me.inputs[", i, "]"])

scycle :: Float -> Float -> Tree Float
scycle a b = float b !* ((float a !* seconds) !% float 1)

sincycle :: Float -> Float -> Tree Float
sincycle a b =float b !* ((osin' $ float a !* seconds) !% float 1)

floor :: (Num n) => Tree n -> Tree n
floor = pyMathOp "floor"

ceil :: (Num n) => Tree n -> Tree n
ceil = pyMathOp "ceil"

osin :: (Num n) => Tree n -> Tree n
osin = pyMathOp "sin"
osin' = (!* float 0.5) . (!+ float 1) . osin

ocos :: (Num n) => Tree n -> Tree n
ocos = pyMathOp "cos"
ocos' = (!* float 0.5) . (!+ float 1) . ocos

pmax :: (Num n) => Tree n -> Tree n -> Tree n
pmax = Mod2 (\s t -> BS.concat ["max(", s, ", ", t, ")"])

pyMathOp :: (Num n) => String -> Tree n -> Tree n
pyMathOp s = Mod (\n -> BS.concat ["math.", pack s, "(", n, ")"])

chan :: Int -> Tree CHOP -> Tree Channel
chan n = Mod (\c -> BS.concat [c, "[", pack $ show n, "]"])

chan0 :: Tree CHOP -> Tree Channel
chan0 = chan 0

chanName :: String -> Tree CHOP -> Tree Channel
chanName s = Mod (\c -> BS.concat [c, "[\"", pack s, "\"]"])

chanf :: Int -> Tree CHOP -> Tree Float
chanf = (fmap . fmap) castf chan

chan0f :: Tree CHOP -> Tree Float
chan0f = castf . chan0

chanNamef :: String -> Tree CHOP -> Tree Float
chanNamef = (fmap . fmap) castf chanName

chanSample :: Tree Int -> Tree Channel -> Tree Float
chanSample = Mod2 (\i c -> BS.concat [c, "[", i, "]"])

numRows :: Tree DAT -> Tree Int
numRows = Mod (\d -> BS.concat [d, ".numRows"])

-- Op helpers


fix :: (Op a) => BS.ByteString -> Tree a -> Tree a
fix = Fix

feedbackC :: Tree CHOP -> (Tree CHOP -> Tree CHOP) -> (Tree CHOP -> Tree CHOP) -> Tree CHOP
feedbackC = FC $ FeedbackCHOP Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] []

feedbackT :: Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP) -> Tree TOP
feedbackT = FT $ FeedbackTOP Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] []

bcomppars = BaseCOMP Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] []

tox :: (Op a, Op b) => String -> [(ByteString, Tree ByteString)] -> Tree a -> Tree b
tox t ps = Comp (bcomppars & baseCOMPexternaltox ?~ (bstr t))


-- CHOPs

-- analyze :: Tree Int -> Tree CHOP -> Tree CHOP
-- analyze f c = N $ Analyze f [c]


-- audioDevOut' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
-- audioDevOut' f = N <$> f . AudioDeviceOut Nothing . (:[])
-- audioDevOut = audioDevOut' id

-- audioFileIn' :: (CHOP -> CHOP) -> Tree ByteString -> Tree CHOP
-- audioFileIn' f file = N . f $ (AudioFileIn file Nothing Nothing Nothing Nothing)
-- audioFileIn = audioFileIn' id

-- audioMovie :: Tree TOP -> Tree CHOP
-- audioMovie movieTop = N $ AudioMovie movieTop

-- audioIn :: Tree CHOP
-- audioIn = N $ AudioIn


-- lowPass :: Tree CHOP -> Tree CHOP
-- lowPass t = N $ AudioFilter (int 0) Nothing [t]

-- highPass :: Tree CHOP -> Tree CHOP
-- highPass t = N $ AudioFilter (int 1) Nothing [t]

-- bandPass :: Tree Float -> Tree CHOP -> Tree CHOP
-- bandPass b t = N $ AudioFilter (int 2) (Just (b !* float 4.5)) [t]

-- audioSpectrum :: Tree CHOP -> Tree CHOP
-- audioSpectrum t = N $ AudioSpectrum [t]

-- constC' :: (CHOP -> CHOP) -> [Tree Float] -> Tree CHOP
-- constC' f fs = N . f $ ConstantCHOP fs Nothing
-- constC = constC' id

-- count' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
-- count' f t = N ins
--   where
--     def = f $ Count [t] Nothing Nothing Nothing Nothing Nothing Nothing
--     ins = def & chopIns %~ (flip (++) (catMaybes . maybeToList $ def ^? countReset))
-- count = count' id

-- datToC' :: (CHOP -> CHOP) -> Tree DAT -> Tree CHOP
-- datToC' f = N . f <$> DATToCHOP Nothing Nothing Nothing
-- datToC = datToC' id


-- delay :: Tree Int -> Tree CHOP -> Tree CHOP
-- delay f = N <$> Delay f . (:[])

-- deleteCNum' :: (CHOP -> CHOP) -> Tree ByteString -> Tree CHOP -> Tree CHOP
-- deleteCNum' f d = N . f <$> DeleteCHOP (Just d) Nothing . (:[])


-- expressionC :: [Tree Float] -> [Tree CHOP] -> Tree CHOP
-- expressionC es is = N $ ExpressionCHOP es is

-- feedbackC :: Tree CHOP -> (Tree CHOP -> Tree CHOP) -> (Tree CHOP -> Tree CHOP) -> Tree CHOP
-- feedbackC = FC (FeedbackCHOP [])


-- fan' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
-- fan' f = N <$> f . Fan Nothing Nothing . (:[])
-- fan = fan' id

-- hold' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP -> Tree CHOP
-- hold' f h t = N <$> f $ Hold [h, t]
-- hold = hold' id

-- lag' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
-- lag' f = N . f <$> Lag emptyV2 Nothing . (:[])
-- lag :: Tree Float -> Tree Float -> Tree CHOP -> Tree CHOP
-- lag a b t = N $ Lag (Just a, Just b) Nothing [t]

-- leapmotion' :: (CHOP -> CHOP) -> Tree CHOP
-- leapmotion' f = N . f $ LeapMotion Nothing Nothing
-- leapmotion = leapmotion' id

-- limitC' :: (CHOP -> CHOP) -> Tree Int -> Tree Float -> Tree Float -> Tree CHOP -> Tree CHOP
-- limitC' f t min max = N <$> f . Limit t min max Nothing . (:[])
-- limitC = limitC' id

-- logic' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
-- logic' f = N <$> f . Logic Nothing Nothing
-- logic = logic' id

-- lookupC :: Tree CHOP -> Tree CHOP -> Tree CHOP
-- lookupC a b = N $ LookupCHOP [a, b]

-- math' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
-- math' f = N <$> f . Math Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing emptyV2 emptyV2


-- mergeC' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
-- mergeC' f = N . f <$> MergeCHOP Nothing Nothing
-- mergeC = mergeC' id

-- mchan :: String -> Tree Float
-- mchan s = chanNamef s $ N MidiIn

-- mselect :: String -> Tree CHOP
-- mselect s = selectC' (selectCNames ?~ str s) $ N MidiIn

-- noiseC' :: (CHOP -> CHOP) -> Tree CHOP
-- noiseC' f = N (f $ NoiseCHOP Nothing emptyV3 Nothing Nothing Nothing Nothing Nothing Nothing)
-- noiseC = noiseC' id

-- nullC' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
-- nullC' f = N <$> f . NullCHOP Nothing . (:[])
-- nullC = nullC' id
-- cookC = nullC' (nullCCookType ?~ int 1)

-- oscinC :: Int -> Tree CHOP
-- oscinC p = N $ OscInCHOP (Resolve $ int p)


-- opsadd :: CHOP -> CHOP
-- opsadd = mathCombChops .~ Just (int 1)

-- opaddf :: Float -> CHOP -> CHOP
-- opaddf a = mathAdd .~ Just (float a)


-- opmultf :: Float -> CHOP -> CHOP
-- opmultf a = mathMult .~ Just (float a)

-- outC :: Tree CHOP -> Tree CHOP
-- outC = N <$> OutCHOP . (:[])

-- renameC' :: (CHOP -> CHOP) -> Tree ByteString -> Tree CHOP -> Tree CHOP
-- renameC' f newName = N . f <$> RenameCHOP newName Nothing . (:[])
-- renameC = renameC' id

-- reorderC' :: (CHOP -> CHOP) -> Tree Int -> Tree CHOP -> Tree CHOP
-- reorderC' f m = N . f <$> Reorder m Nothing . (:[])

-- replaceC :: [Tree CHOP] -> Tree CHOP
-- replaceC = N <$> ReplaceCHOP

-- resampleC' :: (CHOP -> CHOP) -> Bool -> Tree CHOP -> Tree CHOP
-- resampleC' f tc = N . f <$> ResampleCHOP Nothing Nothing (Just $ bool tc) . (:[])

-- scriptC :: String -> [Tree CHOP] -> Tree CHOP
-- scriptC file = N <$> ScriptCHOP (fileD file)

-- scriptCDAT :: Tree DAT -> [Tree CHOP] -> Tree CHOP
-- scriptCDAT dat = N <$> ScriptCHOP dat

-- selectC' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
-- selectC' f c = N . f $ SelectCHOP Nothing (Just c) []
-- selectC = selectC' id

-- selectCConnect' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
-- selectCConnect' f = N . f <$> SelectCHOP Nothing Nothing . (:[])

-- shiftC :: Tree Int -> Tree CHOP -> Tree CHOP
-- shiftC s = N <$> ShiftCHOP s . (:[])

-- shuffleC :: Tree Int -> Tree CHOP -> Tree CHOP
-- shuffleC s = N <$> ShuffleCHOP s . (:[])

-- sopToC :: Tree SOP -> Tree CHOP
-- sopToC = N <$> SOPToCHOP


-- speedC :: Tree CHOP -> Maybe (Tree CHOP) -> Tree CHOP
-- speedC s r = N $ SpeedCHOP (s:(catMaybes [r]))

-- stretchC :: Tree Int -> Tree CHOP -> Tree CHOP
-- stretchC i = N <$> StretchCHOP i . (:[])


-- switchC :: Tree Int -> [Tree CHOP] -> Tree CHOP
-- switchC i = N <$> SwitchCHOP i

-- topToC' :: (CHOP -> CHOP) -> Tree TOP -> Tree CHOP
-- topToC' f = N . f <$> TOPToCHOP Nothing Nothing Nothing Nothing Nothing Nothing
-- topToC = topToC' id

-- trailC' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
-- trailC' f i = N . f $ Trail Nothing Nothing Nothing [i]

-- waveC' :: (CHOP -> CHOP) -> Tree Int -> Tree Float -> Tree CHOP
-- waveC' f e ex = N . f $ WaveCHOP ex e Nothing Nothing
-- waveC = waveC' id

-- data TimerSegment = TimerSegment { segDelay :: Float
--                                  , segLength :: Float
--                                  }


-- timerBS :: TimerSegment -> [ByteString]
-- timerBS (TimerSegment {..}) = [pack $ show segDelay, pack $ show segLength]

-- timerSeg' :: (CHOP -> CHOP) -> [TimerSegment] -> Tree CHOP
-- timerSeg' f ts = N . f $ Timer (Just $ table . fromLists $ ["delay", "length"]:(timerBS <$> ts)) Nothing Nothing Nothing Nothing Nothing Nothing False False False Nothing Nothing Nothing Nothing
-- timerSeg = timerSeg' id

-- timerF' :: (CHOP -> CHOP) -> Tree Int -> Tree CHOP
-- timerF' f l = N . f $ Timer Nothing Nothing Nothing (Just $ int 1) (Just l) Nothing Nothing False False False Nothing Nothing Nothing Nothing

-- timerS' :: (CHOP -> CHOP) -> Tree Float -> Tree CHOP
-- timerS' f l = N . f $ Timer Nothing Nothing Nothing (Just $ int 2) Nothing (Just l) Nothing False False False Nothing Nothing Nothing Nothing

-- timeslice :: Tree CHOP -> Tree CHOP
-- timeslice c = N $ TimeSlice [c]

-- timeline :: Tree CHOP
-- timeline = N Timeline

-- -- DATs

-- arduino :: String -> Int -> Tree DAT
-- arduino p b = N $ SerialDAT (Just $ int b) p (Just $ int 0) Nothing

-- cell :: (Integral a, Integral b) => (Tree a, Tree b) -> Tree DAT -> Tree BS.ByteString
-- cell (r, c) = Mod3 (\r' c' d -> BS.concat [d, "[", r', ",", c', "]"]) r c

-- chopExec' :: (DAT -> DAT) -> Tree CHOP -> Tree DAT
-- chopExec' f chop = N $ f $ ChopExec chop Nothing Nothing Nothing Nothing Nothing

-- chopToD :: Tree CHOP -> Tree DAT
-- chopToD c = N $ CHOPToDAT c

-- datExec' :: (DAT -> DAT) -> Tree DAT -> Tree DAT
-- datExec' f d = N $ f $ DatExec d Nothing Nothing []

-- executeD' :: (DAT -> DAT) -> [Tree DAT] -> Tree DAT
-- executeD' f d = N $ f $ ExecuteDAT Nothing Nothing Nothing Nothing Nothing Nothing d []

-- fileD' :: (DAT -> DAT) -> String -> Tree DAT
-- fileD' f file = N . f $ (TextDAT Nothing (Just . PyExpr $ BS.pack ("\"" ++ file ++ "\"")) [])
-- fileD = fileD' id

-- oscinD' :: (DAT -> DAT) -> Int -> Tree DAT
-- oscinD' f p = N . f $ OscInDAT (Resolve $ int p) (Just $ bool True) (Just $ bool True) (Just $ bool True) Nothing
-- oscinD = oscinD' id

-- scriptD' :: (DAT -> DAT) -> String -> [Tree DAT] -> Tree DAT
-- scriptD' f file = N . f <$> ScriptDAT (fileD file) []
-- scriptD = scriptD' id

-- selectD' :: (DAT -> DAT) -> Tree DAT -> Tree DAT
-- selectD' f t = N . f $ SelectDAT Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing t

-- table :: Matrix BS.ByteString -> Tree DAT
-- table t = N $ Table (Just t) Nothing

-- tableF :: String -> Tree DAT
-- tableF f = N $ Table Nothing (Just $ str f)

-- tcpipD' :: (DAT -> DAT) -> Tree DAT -> Tree DAT
-- tcpipD' f d = N . f $ TCPIPDAT Nothing d Nothing []

-- textD' :: (DAT -> DAT) -> String -> Tree DAT
-- textD' f t = N . f $ TextDAT (Just $ BS.pack t) Nothing []
-- textD = textD' id


-- -- MATs

-- constM' :: (MAT -> MAT) -> Tree MAT
-- constM' f = N . f $ ConstantMAT emptyV3 Nothing Nothing

-- topM :: Tree TOP -> Tree MAT
-- topM t = constM' (constMatMap ?~ t)

-- wireframeM' :: (MAT -> MAT) -> Tree MAT
-- wireframeM' f = N . f $ WireframeMAT emptyV3
-- wireframeM = wireframeM' id

-- pbrM' :: (MAT -> MAT) -> Tree MAT
-- pbrM' f = N . f $ PBRMAT Nothing Nothing Nothing Nothing


-- -- SOPs

-- boxS' :: (SOP -> SOP) -> Tree SOP
-- boxS' f = N . f $ BoxSOP emptyV3

-- chopToS' :: (SOP -> SOP) -> Tree CHOP -> Maybe (Tree SOP) -> Tree SOP
-- chopToS' f c i = N . f $ CHOPToSOP c Nothing Nothing Nothing (maybeToList i)
-- chopToS = chopToS' id

-- circleS' :: (SOP -> SOP) -> Tree SOP
-- circleS' f = N . f $ CircleSOP Nothing Nothing []
-- circleS = circleS' id

-- gridS' :: (SOP -> SOP) -> Tree SOP
-- gridS' f = N . f $ GridSOP Nothing Nothing Nothing Nothing
-- gridS = gridS' id

-- lineS :: Tree SOP
-- lineS = N $ LineSOP

-- mergeS :: [Tree SOP] -> Tree SOP
-- mergeS = N . MergeSOP

-- metaball' :: (SOP -> SOP) -> Tree SOP
-- metaball' f = N . f $ Metaball emptyV3 emptyV3

-- noiseS' :: (SOP -> SOP) -> Tree SOP -> Tree SOP
-- noiseS' f = N <$> f . NoiseSOP emptyV3 . (:[])
-- noiseS = noiseS' id

-- outS :: Tree SOP -> Tree SOP
-- outS = N <$> OutSOP . (:[])

-- scaleS :: Tree Float -> Tree SOP -> Tree SOP
-- scaleS f s = transformS' (transformSUniformScale ?~ f) s

-- sphere' :: (SOP -> SOP) -> Tree SOP
-- sphere' f = N . f $ Sphere Nothing []
-- sphere = sphere' id

-- torus' :: (SOP -> SOP) -> Tree SOP
-- torus' f = N . f $ Torus Nothing Nothing Nothing Nothing emptyV2 []
-- torus = torus' id

-- transformS' :: (SOP -> SOP) -> Tree SOP -> Tree SOP
-- transformS' f = N <$> f . (TransformSOP Nothing emptyV3 emptyV3) . (:[])
-- transformS = transformS' id

-- tubeS' :: (SOP -> SOP) -> Tree SOP
-- tubeS' f = N . f $ TubeSOP emptyV2 Nothing

-- -- TOPs

-- blur' :: (TOP -> TOP) -> Tree Float -> Tree TOP -> Tree TOP
-- blur' f b t = N . f $ Blur b [t] Nothing
-- blur = blur' id

-- chopToT' :: (TOP -> TOP) -> Tree CHOP -> Tree TOP
-- chopToT' f c = N . f $ CHOPToTOP c Nothing
-- chopToT = chopToT' id

-- circleT = circleT' id
-- circleT' :: (TOP -> TOP) -> Tree TOP
-- circleT' f = N . f $ CircleTOP

-- compT' :: (TOP -> TOP) -> Int -> [Tree TOP] -> Tree TOP
-- compT' f op ts = N . f $ CompositeTOP (int op) ts Nothing emptyV2
-- compT = compT' id

-- crop' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
-- crop' f = N . f <$> Crop Nothing Nothing Nothing Nothing . (:[])

-- displace :: Tree TOP -> Tree TOP -> Tree TOP
-- displace a b = N $ Displace [a, b]

-- edges' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
-- edges' f a = N . f $ Edges [a] Nothing
-- edges = edges' id

-- feedbackT :: Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP) -> Tree TOP
-- feedbackT = FT (FeedbackTOP Nothing [])

-- flipT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
-- flipT' f t = N . f $ Flip Nothing Nothing Nothing [t] Nothing


-- glslT' :: (TOP -> TOP) -> String -> [Tree TOP] -> Tree TOP
-- glslT' f d ts = N . f $ GLSLTOP (fileD d) [] Nothing emptyV2 ts Nothing
-- glslT = glslT' id

-- glslTP' :: (TOP -> TOP) -> String -> [(String, Vec4)] -> [Tree TOP] -> Tree TOP
-- glslTP' f s us ts = glslT' ((glslTUniforms .~ us) . f) s ts
-- glslTP = glslTP' id

-- hsvT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
-- hsvT' f = N <$> f. HSVAdjust Nothing Nothing Nothing . (:[])

-- levelT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
-- levelT' f = N <$> f. LevelTOP Nothing Nothing Nothing . (:[])

-- movieFileIn = movieFileIn' id
-- movieFileIn' :: (TOP -> TOP) -> Tree ByteString -> Tree TOP
-- movieFileIn' f file = N . f $ (MovieFileIn file Nothing Nothing emptyV2)

-- ndiinT :: String -> Tree TOP
-- ndiinT n = N $ (NdiInTOP (str n))


-- noiseT' :: (TOP -> TOP) -> Tree TOP
-- noiseT' f = N $ f $ NoiseTOP Nothing Nothing emptyV2 emptyV3 Nothing Nothing Nothing Nothing Nothing

-- nullT :: Tree TOP -> Tree TOP
-- nullT = N . NullTOP . (:[])

-- outT :: Tree TOP -> Tree TOP
-- outT = N <$> OutTOP . (:[])

-- ramp' :: (TOP -> TOP) -> Tree DAT -> Tree TOP
-- ramp' f = N . f <$> (Ramp Nothing Nothing emptyV2)

-- rampC' :: (TOP -> TOP) -> [(Float, Float, Float, Float, Float)] -> Tree TOP
-- rampC' f = ramp' f . table . fromLists . fmap (^..each) . ((:) ("pos", "r", "g", "b", "a")) . fmap ((over each) (BS.pack . show))

-- rectangle' :: (TOP -> TOP) -> Vec2 -> Tree TOP
-- rectangle' f size = N . f $ RectangleTOP size emptyV2 emptyV3 emptyV3 Nothing emptyV2
-- rectangle = rectangle' id

-- render = render' id
-- render' :: (TOP -> TOP) -> [Tree Geo] -> Tree Camera -> Tree TOP
-- render' f geos cam = N . f $ Render geos cam [] Nothing

-- selectT :: Tree TOP -> Tree TOP
-- selectT = N <$> SelectTOP . Just

-- switchT' :: (TOP -> TOP) -> Tree Float -> [Tree TOP] -> Tree TOP
-- switchT' f i = N . f <$> SwitchTOP i Nothing
-- switchT = switchT' id

-- textT' :: (TOP -> TOP) -> Tree ByteString -> Tree TOP
-- textT' f tx = N . f $ TextTOP tx emptyV3 Nothing emptyV2 emptyV2
-- textT = textT' id

-- transformT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
-- transformT' f = N <$> f . (TransformTOP emptyV2 Nothing emptyV2 Nothing Nothing emptyV2) . (:[])
-- transformT = transformT' id

-- vidIn :: Tree TOP
-- vidIn = N $ VideoDeviceIn


-- -- COMPs

-- geo' :: (Geo -> Geo) -> Tree SOP -> Tree Geo
-- geo' f = Comp (f $ Geo emptyV3 emptyV3 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

-- instanceGeo' :: (Geo -> Geo) -> Tree CHOP -> Tree SOP -> Tree Geo
-- instanceGeo' f c = geo' (f
--                          . (geoInstanceTX ?~ str "tx") . (geoInstanceTY ?~ str "ty") . (geoInstanceTZ ?~ str "tz")
--                          . (geoInstanceRX ?~ str "rx") . (geoInstanceRY ?~ str "ry") . (geoInstanceRZ ?~ str "rz")
--                          . (geoInstanceSX ?~ str "sx") . (geoInstanceSY ?~ str "sy") . (geoInstanceSZ ?~ str "sz")
--                          . (geoInstanceChop ?~ c))

-- cam' :: (Camera -> Camera) -> Tree Camera
-- cam' f = N . f $ Camera emptyV3 emptyV3 emptyV3 Nothing
-- cam = cam' id

-- light' :: (Light -> Light) -> Tree Light
-- light' f = N . f $ Light Nothing emptyV3 emptyV3 Nothing Nothing Nothing Nothing Nothing
-- light = light' id

-- base :: (Baseable a, Baseable b) => (Tree a -> Tree b) -> Tree a -> Tree b
-- base = BComp $ BaseCOMP [] Nothing

-- tox :: (Op a, Op b) => String -> [(ByteString, Tree ByteString)] -> Maybe (Tree a) -> Tree b
-- tox t ps = Tox $ BaseCOMP ps (Just $ str t)
