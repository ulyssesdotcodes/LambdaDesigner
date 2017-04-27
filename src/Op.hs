{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Op where

import Prelude hiding (sin)

import Tree

import Control.Lens
import Data.Matrix
import Data.Maybe

import Data.ByteString.Char8 as BS
import Data.Map.Strict as M

data CommandType = Pulse ByteString deriving Eq

class Op a where
  opType :: a -> ByteString
  opPars :: a -> Map ByteString (Param (TDType ByteString))
  opText :: a -> Maybe ByteString
  opCommands :: a -> [CommandType]

-- Tree

data Tree a where
  GeneratorTree :: (Op a) => a -> Tree a
  EffectTree :: (Op a) => a -> Tree a -> Tree a
  CombineTree :: (Op a) => a -> Tree a -> Tree a -> Tree a
  CompositeTree :: (Op a) => a -> [Tree a] -> Tree a

  FeedbackTopTree :: TOP -> Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP) -> Tree TOP
  FeedbackChopTree :: CHOP -> Tree CHOP -> (Tree CHOP -> Tree CHOP) -> (Tree CHOP -> Tree CHOP) -> Tree CHOP

  ComponentTree :: (Op a, Op b) => a -> Tree b -> Tree a

  FixedTree :: (Op a) => ByteString -> Tree a -> Tree a

data Param b where
  TDParam :: TDType a -> Param (TDType a)
  TreeParam :: Tree a -> Param (Tree a)
  CHOPChan :: Int -> Param (Tree CHOP) -> Param (TDType TDFloat)
  CHOPChanName :: String -> Param (Tree CHOP) -> Param (TDType TDFloat)
  SampleTOP :: (Int, Int) -> Param (Tree TOP) -> Param (Tree CHOP)
  ResolveT :: Param a -> Param (TDType ByteString)

pars :: Lens' (Tree a) a
pars f (CombineTree t o1 o2) = fmap (\t' -> CombineTree t' o1 o2) (f t)
pars f (CompositeTree t os) = fmap (\t' -> CompositeTree t' os) (f t)
pars f (GeneratorTree a) = fmap (\a' -> GeneratorTree a') (f a)
pars f (EffectTree a aop) = fmap (\a' -> EffectTree a' aop) (f a)
pars f (FeedbackTopTree a b c d) = fmap (\a' -> FeedbackTopTree a' b c d) (f a)
pars f (FeedbackChopTree a b c d) = fmap (\a' -> FeedbackChopTree a' b c d) (f a)
pars f (ComponentTree a aop) = fmap (\a' -> ComponentTree a' aop) (f a)
pars f (FixedTree _ aop) = pars f aop

-- Parameters

data CHOP = NoiseCHOP { _noiseCTranslate :: Vec3
                      , _noiseCRoughness :: Param (TDType TDFloat)
                      , _noiseCType :: Param (TDType TDInt)
                      , _noiseCTimeSlice :: Param Bool
                      , _noiseCPeriod :: Param (TDType TDFloat)
                      }
          | SOPToCHOP { _sopToChopSop :: Param (Tree SOP) }
          | Logic { _logicPreop :: Param Int
                  , _logicConvert :: Param Int
                  }
          | Hold
          | ConstantCHOP { _name0 :: Param BS.ByteString
                         , _value0 :: Param (TDType TDFloat)
                         }
          | FeedbackCHOP
          | SelectCHOP { _selectCChop :: Param (Tree CHOP)
                       }
          | Count { _countThresh :: Param (TDType TDFloat)
                  , _countLimType :: Param Int
                  , _countLimMin :: Param (TDType TDFloat)
                  , _countLimMax :: Param (TDType TDFloat)
                  }
          | MergeCHOP
          | Fan { _fanOp :: Param Int
                , _fanOffNeg :: Param Bool
                }
          | Math { _mathAdd :: Param (TDType TDFloat)
                 , _mathCombChops :: Param Int
                 }


data TOP = CHOPToTOP { _chopToTopChop :: Param (Tree CHOP) }
           | Displace
           | MovieFileIn { _movieFileInFile :: Param BS.ByteString
                         , _moviePlayMode :: Param Int
                         , _movieIndex :: Param Int
                         }
           | OutTOP
           | NullTOP
           | Render { _renderGeo :: Param (Tree COMP)
                   , _renderCamera :: Param (Tree COMP)
                   , _renderLight :: Param (Tree COMP)
                   }
           | FeedbackTOP
           | Transform { _transformTranslate :: Vec2
                       , _transformScale :: Vec2
                       }
           | CompositeTOP { _compTOperand :: Param Int }
           | CircleTOP
           | LevelTOP { _levelOpacity :: Param (TDType TDFloat)
                      }
           | NoiseTOP { _noiseTMonochrome :: Param Bool
                      , _noiseTResolution :: Dimen
                      , _noiseTTranslate :: Vec3
                      }
           | Ramp { _rampType :: Param Int
                  , _rampPhase :: Param (TDType TDFloat)
                  , _rampResolution :: Dimen
                  , _rampValues :: Param (Tree DAT)
                  }
           | SwitchTOP { _switchTIndex :: Param (TDType TDFloat)
                       }
           | SelectTOP { _selectTTop :: Param (Tree TOP)
                       }

data SOP = Sphere
         | OutSOP
         | CircleSOP { _circType :: Param Int
                     , _circArc :: Param Int
                     }
         | NoiseSOP { _noiseSTranslate :: Vec3 }
         | CHOPToSOP { _chopToSopChop :: Param (Tree CHOP)
                     , _chopToSopAttrScope :: Param BS.ByteString
                     }

data MAT = ConstantMAT { _constColor :: RGB
                       , _constAlpha :: Param (TDType TDFloat)
                       }

data DAT = Table { _tableText :: Matrix BS.ByteString
                 }
         | ChopExec { _chopExecChop :: Param (Tree CHOP)
                    , _ceOffToOn :: Maybe BS.ByteString
                    , _ceWhileOn :: Maybe BS.ByteString
                    , _ceOnToOff :: Maybe BS.ByteString
                    , _ceWhileOff :: Maybe BS.ByteString
                    , _ceValueChange :: Maybe BS.ByteString
                    }
         | TextDAT { _text :: Maybe BS.ByteString
                   , _textFile :: Param BS.ByteString
                   }
         | TCPIPDAT { _tcpipMode :: Param Int
                    , _tcpipCallbacks :: Param (Tree DAT)
                    , _tcpipCallbackFormat :: Param Int
                    }

data COMP = Geo { _geoTranslate :: Vec3
                , _geoScale :: Vec3
                , _geoMat :: Param (Tree MAT)
                , _geoUniformScale :: Param (TDType TDFloat)
                }
          | Camera { _camTranslate :: Vec3
                   }
          | Light


makeLenses ''CHOP
makeLenses ''TOP
makeLenses ''SOP
makeLenses ''COMP
makeLenses ''MAT
makeLenses ''DAT

-- Chops

instance Op CHOP where
  opPars (NoiseCHOP t r nt ts p) = M.union (fromListMaybe [ ("roughness", ResolveT r)
                                                          , ("type", ResolveT nt)
                                                          , ("channelname", chan)
                                                          , ("timeslice", ResolveT ts)
                                                          , ("period", ResolveT p)
                                                          ]) $ vec3Map' "t" t
  opPars (SOPToCHOP s) = M.singleton "sop" $ Resolve s
  opPars (Logic p c) = fromListMaybe [("preop", ResolveT p), ("convert", ResolveT c)]
  opPars (Hold) = M.empty
  opPars (ConstantCHOP n v) = M.union (fromListMaybe [("name0", ResolveT n)]) $ M.fromList [("value0", Resolve v)]
  opPars (FeedbackCHOP) = M.empty
  opPars (SelectCHOP c) = M.singleton "chop" (Resolve c)
  opPars (Count t l min max) = fromListMaybe [("threshup", (ResolveT t)), ("output", ResolveT l), ("limitmin", ResolveT min), ("limitmax", ResolveT max)]
  opPars (Fan o n) = fromListMaybe [("fanop", (ResolveT o)), ("alloff", ResolveT n)]
  opPars (MergeCHOP) = M.empty
  opPars (Math a c) = fromListMaybe [("preoff", ResolveT a), ("chopop", ResolveT c)]
  opType (NoiseCHOP _ _ _ _ _) = "noiseCHOP"
  opType (SOPToCHOP _) = "sopToChop"
  opType (Logic _ _) = "logic"
  opType (Hold) = "hold"
  opType (ConstantCHOP _ _) = "constantChop"
  opType (FeedbackCHOP) = "feedbackChop"
  opType (SelectCHOP _) = "selectChop"
  opType (Count _ _ _ _) = "count"
  opType (Fan _ _) = "fan"
  opType (MergeCHOP) = "mergeChop"
  opType (Math _ _) = "math"
  opText _ = Nothing
  opCommands (Count _ _ _ _) = [Pulse "reset"]
  opCommands _ = []

tp :: Tree a -> Param (Tree a)
tp = TreeParam

chopChan :: Int -> Param (Tree CHOP) -> Param (TDType TDFloat)
chopChan i = CHOPChan i . tp

chopChanName :: String -> Tree CHOP -> Param (TDType TDFloat)
chopChanName n = CHOPChanName n . tp

chopChan0 :: Tree CHOP -> Param (TDType TDFloat)
chopChan0 = chopChan 0

sampleTop :: Tree TOP -> Param (TDType TDFloat)
sampleTop = SampleTOP p i . tp

-- (\opstring -> BS.append opstring (BS.pack $ ".sample(x=" ++ show x ++ ",y=" ++ show y ++ ")[" ++ show n ++ "]")) . TreePar

noiseChop :: Tree CHOP
noiseChop = GeneratorTree (NoiseCHOP emptyV3 Nothing Nothing Nothing Nothing Nothing)

sopToChop :: Tree SOP -> Tree CHOP
sopToChop = GeneratorTree . SOPToCHOP . TreePar

logic :: Tree CHOP -> Tree CHOP
logic = EffectTree (Logic Nothing Nothing)

hold :: Tree CHOP -> Tree CHOP -> Tree CHOP
hold = CombineTree Hold

constChop :: Param (TDType TDFloat) -> Tree CHOP
constChop = GeneratorTree . ConstantCHOP Nothing

feedbackChop :: Tree CHOP -> (Tree CHOP -> Tree CHOP) -> (Tree CHOP -> Tree CHOP) -> Tree CHOP
feedbackChop r t = FeedbackTree FeedbackCHOP r t

selectChop :: Tree CHOP -> Tree CHOP
selectChop = GeneratorTree <$> SelectCHOP . treePar

count :: Tree CHOP -> Tree CHOP
count = EffectTree (Count Nothing Nothing Nothing Nothing)

countReset :: Tree CHOP -> Tree CHOP -> Tree CHOP
countReset = CombineTree (Count Nothing Nothing Nothing Nothing)

fan :: Tree CHOP -> Tree CHOP
fan = EffectTree (Fan Nothing Nothing)

mergeChop :: [Tree CHOP] -> Tree CHOP
mergeChop = CompositeTree MergeCHOP

math :: [Tree CHOP] -> Tree CHOP
math = CompositeTree (Math Nothing Nothing)

opsadd :: [Tree CHOP] -> Tree CHOP
opsadd = math <&> pars.mathCombChops .~ Just (int 1)

opaddf :: Float -> Tree CHOP -> Tree CHOP
opaddf a = (math <&> pars.mathAdd.~Just (float a)) . (:[])

-- Tops

instance Op TOP where
  opPars (CHOPToTOP chop) = M.singleton (BS.pack "chop") (Resolve chop)
  opPars (CompositeTOP op) = fromListMaybe [("operand", Just $ Resolve op)]
  opPars (FeedbackTOP) = M.empty
  opPars (MovieFileIn file mode index) = M.union (fromListMaybe [ ("playmode", ResolveT mode)
                                                                , ("index", ResolveT index)]) $ M.singleton (BS.pack "file") file
  opPars (Render geo cam light) = fromListMaybe [ ("geometry", Just $ Resolve geo)
                                                , ("camera", Just $ Resolve cam)
                                                , ("lights", ResolveT light)]
  opPars (Transform t s) = M.union (vec2Map' "t" t) (vec2Map' "s" s)
  opPars (LevelTOP o) = fromListMaybe [("opacity", ResolveT o)]
  opPars (NoiseTOP m r t) = M.unions  [(fromListMaybe [("mono", ResolveT m)]), (dimenMap "resolution" r), vec3Map' "t" t]
  opPars (SwitchTOP i) = M.singleton "index" (Resolve i)
  opPars CircleTOP = M.empty
  opPars Displace = M.empty
  opPars OutTOP = M.empty
  opPars NullTOP = M.empty
  opPars (Ramp t p r dat) = M.union (dimenMap "resolution" r) $ fromListMaybe [("dat", Just (Resolve $ dat)), ("type", ResolveT t), ("phase", ResolveT p)]
  opPars (SelectTOP c) = M.singleton "top" (Resolve c)

  opType (CHOPToTOP _) = "chopToTop"
  opType (CompositeTOP _) = "compositeTop"
  opType (Displace) = "displace"
  opType (MovieFileIn _ _ _) = "movieFileIn"
  opType (Render _ _ _) = "render"
  opType (Transform _ _) = "transform"
  opType CircleTOP = "circleTop"
  opType (FeedbackTOP) = "feedbackTop"
  opType OutTOP = "outTop"
  opType NullTOP = "nullTop"
  opType (LevelTOP _) = "levelTop"
  opType (NoiseTOP _ _ _) = "noiseTop"
  opType (Ramp _ _ _ _) = "ramp"
  opType (SwitchTOP _) = "switchTop"
  opType (SelectTOP _) = "selectTop"

  opText _ = Nothing
  opCommands _ = []

movieFileIn :: Param BS.ByteString -> Tree TOP
movieFileIn f = GeneratorTree (MovieFileIn f Nothing Nothing)

movieFileIn' :: String -> Tree TOP
movieFileIn' (BS.pack -> file) = movieFileIn $ File file

displace :: Tree TOP -> Tree TOP -> Tree TOP
displace = CombineTree Displace

chopToTop :: Tree CHOP -> Tree TOP
chopToTop = GeneratorTree . CHOPToTOP . TreePar

outTop :: Tree TOP -> Tree TOP
outTop = EffectTree OutTOP

render :: Tree COMP -> Tree COMP -> Tree TOP
render geo cam = GeneratorTree (Render (TreePar geo) (TreePar cam) Nothing)

compTop :: Int -> [Tree TOP] -> Tree TOP
compTop op = CompositeTree (CompositeTOP $ int op)

feedbackTop :: Tree TOP -> (Tree TOP -> Tree TOP) -> Tree TOP
feedbackTop r t = FeedbackTree FeedbackTOP r t selectTop

circleTop :: Tree TOP
circleTop = GeneratorTree CircleTOP

transformTop :: Tree TOP -> Tree TOP
transformTop = EffectTree (Transform emptyV2 emptyV2)

nullTop :: Tree TOP -> Tree TOP
nullTop = EffectTree NullTOP

levelTop :: Tree TOP -> Tree TOP
levelTop = EffectTree (LevelTOP Nothing)

noiseTop :: Tree TOP
noiseTop = GeneratorTree (NoiseTOP Nothing emptyIV2 emptyV3)

ramp :: [(Float, Float, Float, Float, Float)] -> Tree TOP
ramp = GeneratorTree . (Ramp Nothing Nothing emptyIV2) . treePar . table . fromLists . fmap (^..each) . ((:) ("pos", "r", "g", "b", "a")) . fmap ((over each) (BS.pack . show))

switchTop :: Param (TDType TDFloat) -> [Tree TOP] -> Tree TOP
switchTop i = CompositeTree (SwitchTOP i)

selectTop :: Tree TOP -> Tree TOP
selectTop = GeneratorTree <$> SelectTOP . treePar

-- SOPs

instance Op SOP where
  opPars (CircleSOP p a) = fromListMaybe [ ("type", ResolveT p)
                                         , ("arc", ResolveT a)
                                         ]
  opPars (NoiseSOP t) = vec3Map' "t" t
  opPars OutSOP = M.empty
  opPars Sphere = M.empty
  opPars (CHOPToSOP c a) = fromListMaybe [ ("chop", Just $ Resolve c)
                                         , ("attscope", a)
                                         ]
  opType (CircleSOP _ _) = "circleSop"
  opType (NoiseSOP _) = "noiseSop"
  opType OutSOP = "outSop"
  opType Sphere = "sphere"
  opType (CHOPToSOP _ _) = "chopToSop"

  opText _ = Nothing
  opCommands _ = []

circleSop :: Tree SOP
circleSop = GeneratorTree $ CircleSOP Nothing Nothing

noiseSop :: Tree SOP -> Tree SOP
noiseSop = EffectTree $ NoiseSOP emptyV3

sphere :: Tree SOP
sphere = GeneratorTree Sphere

outSop :: Tree SOP -> Tree SOP
outSop = EffectTree OutSOP

chopToSop :: Tree SOP -> Tree CHOP -> Tree SOP
chopToSop s c = EffectTree (CHOPToSOP (treePar c) Nothing) s

-- MATs

instance Op MAT where
  opPars (ConstantMAT rgb alpha) = M.union (fromListMaybe [("alpha", ResolveT alpha)]) $ rgbMap "color" rgb
  opType (ConstantMAT _ _) = "constMat"
  opText _ = Nothing
  opCommands _ = []

constantMat :: Tree MAT
constantMat = GeneratorTree (ConstantMAT emptyV3 Nothing)

-- DATs
instance Op DAT where
  opPars (Table _) = M.empty
  opPars (ChopExec chop offon won onoff woff vc ) = M.fromList [("chop", Resolve chop)
                                                               , ("offtoon", Resolve . B $ isJust offon)
                                                               , ("whileon", Resolve . B $ isJust won)
                                                               , ("ontooff", Resolve . B $ isJust onoff)
                                                               , ("whileoff", Resolve . B $ isJust woff)
                                                               , ("valuechange", Resolve . B $ isJust vc)
                                                               ]
  opPars (TextDAT _ f) = fromListMaybe [("file", f)]
  opPars (TCPIPDAT m d f) = M.union (fromListMaybe [("mode", ResolveT m), ("format", ResolveT f)]) $ M.singleton "callbacks" (Resolve d)
  opType (ChopExec _ _ _ _ _ _) = "chopExec"
  opType (TextDAT _ _) = "textDat"
  opType (Table _) = "table"
  opType (TCPIPDAT _ _ _) = "tcpip"
  opText (Table t) = Just . BS.intercalate ("\n") . fmap (BS.intercalate ("\t")) . toLists $ t
  opText (ChopExec _ offon won onoff woff vc) =
    Just . BS.intercalate "\n\n" . (traverse %~ concatFunc)
      $ catMaybes [ ("offToOn",) <$> offon
                  , ("whileOn",) <$> won
                  , ("onToOff",) <$> onoff
                  , ("whileOff",) <$>  woff
                  , ("valueChange",) <$> vc
                  ]
    where
      concatFunc (name, body) = BS.append (makeChopExecFunc name) body
  opText (TextDAT t _) = t
  opText _ = Nothing
  opCommands (TextDAT _ (isJust -> True)) = [Pulse "loadonstartpulse"]
  opCommands _ = []

makeChopExecFunc :: BS.ByteString -> BS.ByteString
makeChopExecFunc prog = BS.concat ["def ", prog, "(channel, sampleIndex, val, prev): \n"]

table :: Matrix BS.ByteString -> Tree DAT
table = GeneratorTree . Table

chopExec :: Tree CHOP -> Tree DAT
chopExec chop = GeneratorTree $ ChopExec (treePar chop) Nothing Nothing Nothing Nothing Nothing

cell :: (Integral a, Integral b) => (Param a, Param b) -> Tree DAT -> Param BS.ByteString
cell (x, y) t = Cell x y (treePar t)

cellf :: (Integral a, Integral b, Floating f, Show f) => (Param a, Param b) -> Tree DAT -> Param f
cellf (x, y) t = Cell x y (treePar t)

casti :: (Show f, Floating f, Integral i) => Param f -> Param i
casti = Mod (\f -> BS.concat ["int(", f, ")"])

textDat :: String -> Tree DAT
textDat t = GeneratorTree (TextDAT (Just $ BS.pack t) Nothing)

fileDat :: String -> Tree DAT
fileDat f = GeneratorTree (TextDAT Nothing (Just . File $ BS.pack ("\"" ++ f ++ "\"")))

tcpipDat :: Tree DAT -> Tree DAT
tcpipDat d = GeneratorTree (TCPIPDAT Nothing (treePar d) Nothing)

-- COMPs
instance Op COMP where
  opType (Geo _ _ _ _) = "geo"
  opType (Camera _) = "camera"
  opType Light = "light"
  opPars (Geo t s m us) = M.unions [fromListMaybe [("material", ResolveT m), ("scale", ResolveT us)], (vec3Map' "t" t), (vec3Map' "s" s)]
  opPars (Camera t) = vec3Map' "t" t
  opPars Light = M.empty
  opText _ = Nothing
  opCommands _ = []

geo :: Tree SOP -> Tree COMP
geo = ComponentTree (Geo emptyV3 emptyV3 Nothing Nothing)

cam :: Tree COMP
cam = GeneratorTree (Camera emptyV3)

light :: Tree COMP
light = GeneratorTree Light

-- Misc

fix :: (Op a) => BS.ByteString -> Tree a -> Tree a
fix = FixedTree
