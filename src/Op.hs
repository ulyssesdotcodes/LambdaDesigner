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

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M

data CHOP = NoiseCHOP { _noiseCTranslate :: Vec3
                      , _noiseCRoughness :: Maybe (Param Float)
                      , _noiseCType :: Maybe (Param Int)
                      , _noiseCChannel :: Maybe (Param BS.ByteString)
                      , _noiseCTimeSlice :: Maybe (Param Bool)
                      , _noiseCPeriod :: Maybe (Param Float)
                      }
          | SOPToCHOP { _sopToChopSop :: Param (Tree SOP) }
          | Logic { _logicPreop :: Maybe (Param Int)
                  , _logicConvert :: Maybe (Param Int)
                  }
          | Hold
          | ConstantCHOP { _name0 :: Maybe (Param BS.ByteString)
                         , _value0 :: Param Float
                         }


data TOP = CHOPToTOP { _chopToTopChop :: Param (Tree CHOP) }
           | Displace
           | MovieFileIn { _movieFileInFile :: Param BS.ByteString }
           | OutTOP
           | NullTOP
           | Render { _renderGeo :: Param (Tree COMP)
                   , _renderCamera :: Param (Tree COMP)
                   , _renderLight :: Maybe (Param (Tree COMP))
                   }
           | FeedbackTOP
           | Transform { _transformTranslate :: Vec2
                       , _transformScale :: Vec2
                       }
           | CompositeTOP { _compTOperand :: Param Int }
           | CircleTOP
           | LevelTOP { _levelOpacity :: Maybe (Param Float)
                      }
           | NoiseTOP { _noiseTMonochrome :: Maybe (Param Bool)
                      , _noiseTResolution :: Dimen
                      , _noiseTTranslate :: Vec3
                      }
           | Ramp { _rampType :: Maybe (Param Int)
                  , _rampPhase :: Maybe (Param Float)
                  , _rampResolution :: Dimen
                  , _rampValues :: Param (Tree DAT)
                  }

data SOP = Sphere
         | OutSOP
         | CircleSOP { _circType :: Maybe (Param Int)
                     , _circArc :: Maybe (Param Int)
                     }
         | NoiseSOP { _noiseSTranslate :: Vec3 }
         | CHOPToSOP { _chopToSopChop :: Param (Tree CHOP)
                     , _chopToSopAttrScope :: Maybe (Param BS.ByteString)
                     }

data MAT = ConstantMAT { _constColor :: RGB
                       , _constAlpha :: Maybe (Param Float)
                       }

data DAT = Table { _text :: Matrix BS.ByteString
                 }
         | ChopExec { _chopExecChop :: Param (Tree CHOP)
                    , _ceOffToOn :: Maybe BS.ByteString
                    , _ceWhileOn :: Maybe BS.ByteString
                    , _ceOnToOff :: Maybe BS.ByteString
                    , _ceWhileOff :: Maybe BS.ByteString
                    , _ceValueChange :: Maybe BS.ByteString
                    }

data COMP = Geo { _geoTranslate :: Vec3
                , _geoScale :: Vec3
                , _geoMat :: Maybe (Param (Tree MAT))
                , _geoUniformScale :: Maybe (Param Float)
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
  opPars (NoiseCHOP t r nt chan ts p) = M.union (fromListMaybe [ ("roughness", ShowP <$> r)
                                                             , ("type", ShowP <$> nt)
                                                             , ("channelname", chan)
                                                             , ("timeslice", ShowP <$> ts)
                                                             , ("period", ShowP <$> p)
                                                             ]) $ vec3Map' "t" t
  opPars (SOPToCHOP s) = M.singleton "sop" $ ShowP s
  opPars (Logic p c) = fromListMaybe [("preop", ShowP <$> p), ("convert", ShowP <$> c)]
  opPars (Hold) = M.empty
  opPars (ConstantCHOP n v) = M.union (fromListMaybe [("name0", ShowP <$> n)]) $ M.fromList [("value0", ShowP v)]
  opType (NoiseCHOP _ _ _ _ _ _) = "noiseCHOP"
  opType (SOPToCHOP _) = "sopToChop"
  opType (Logic _ _) = "logic"
  opType (Hold) = "hold"
  opType (ConstantCHOP _ _) = "constantChop"
  opText _ = Nothing

chopChan :: Int -> Tree CHOP -> Param Float
chopChan i = TreeFloat (\opstring -> BS.append opstring (BS.pack $ "[" ++ show i ++ "]")) . TreePar

chopChanName :: String -> Tree CHOP -> Param Float
chopChanName n = TreeFloat (\opstring -> BS.append opstring (BS.pack $ "[\'" ++ n ++ "\']")) . TreePar

chopChan0 :: Tree CHOP -> Param Float
chopChan0 = chopChan 0

sampleTop :: Int -> (Int, Int) -> Tree TOP -> Param Float
sampleTop n (x, y) = TreeFloat (\opstring -> BS.append opstring (BS.pack $ ".sample(x=" ++ show x ++ ",y=" ++ show y ++ ")[" ++ show n ++ "]")) . TreePar

noiseCHOP :: Tree CHOP
noiseCHOP = GeneratorTree (NoiseCHOP emptyV3 Nothing Nothing Nothing Nothing Nothing)

sopToChop :: Tree SOP -> Tree CHOP
sopToChop = GeneratorTree . SOPToCHOP . TreePar

logic :: Tree CHOP -> Tree CHOP
logic = EffectTree (Logic Nothing Nothing)

hold :: Tree CHOP -> Tree CHOP -> Tree CHOP
hold = CompositeTree Hold

constChop :: Param Float -> Tree CHOP
constChop = GeneratorTree . ConstantCHOP Nothing

-- Tops

instance Op TOP where
  opPars (CHOPToTOP chop) = M.singleton (BS.pack "chop") (ShowP chop)
  opPars (CompositeTOP op) = fromListMaybe [("operand", Just $ ShowP op)]
  opPars (FeedbackTOP) = M.empty
  opPars (MovieFileIn file) = M.singleton (BS.pack "file") file
  opPars (Render geo cam light) = fromListMaybe [ ("geometry", Just $ ShowP geo)
                                                , ("camera", Just $ ShowP cam)
                                                , ("lights", ShowP <$> light)]
  opPars (Transform t s) = M.union (vec2Map' "t" t) (vec2Map' "s" s)
  opPars (LevelTOP o) = fromListMaybe [("opacity", ShowP <$> o)]
  opPars (NoiseTOP m r t) = M.unions  [(fromListMaybe [("mono", ShowP <$> m)]), (dimenMap "resolution" r), vec3Map' "t" t]
  opPars CircleTOP = M.empty
  opPars Displace = M.empty
  opPars OutTOP = M.empty
  opPars NullTOP = M.empty
  opPars (Ramp t p r dat) = M.union (dimenMap "resolution" r) $ fromListMaybe [("dat", Just (ShowP $ dat)), ("type", ShowP <$> t), ("phase", ShowP <$> p)]

  opType (CHOPToTOP _) = "chopToTop"
  opType (CompositeTOP _) = "compositeTop"
  opType (Displace) = "displace"
  opType (MovieFileIn _) = "movieFileIn"
  opType (Render _ _ _) = "render"
  opType (Transform _ _) = "transform"
  opType CircleTOP = "circleTop"
  opType (FeedbackTOP) = "feedbackTop"
  opType OutTOP = "outTop"
  opType NullTOP = "nullTop"
  opType (LevelTOP _) = "levelTop"
  opType (NoiseTOP _ _ _) = "noiseTop"
  opType (Ramp _ _ _ _) = "ramp"

  opText _ = Nothing

movieFileIn :: Param BS.ByteString -> Tree TOP
movieFileIn f = GeneratorTree (MovieFileIn f)

movieFileIn' :: String -> Tree TOP
movieFileIn' (BS.pack -> file) = movieFileIn $ File file

displace :: Tree TOP -> Tree TOP -> Tree TOP
displace = CompositeTree Displace

chopToTop :: Tree CHOP -> Tree TOP
chopToTop = GeneratorTree . CHOPToTOP . TreePar

outTop :: Tree TOP -> Tree TOP
outTop = EffectTree OutTOP

render :: Tree COMP -> Tree COMP -> Tree TOP
render geo cam = GeneratorTree (Render (TreePar geo) (TreePar cam) Nothing)

compTop :: Int -> Tree TOP -> Tree TOP -> Tree TOP
compTop op = CompositeTree (CompositeTOP $ int op)

feedbackTop :: Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP -> Tree TOP) -> Tree TOP
feedbackTop = FeedbackTree FeedbackTOP

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
ramp = GeneratorTree . (Ramp Nothing Nothing emptyIV2) . treePar . table . fromLists . map (^..each) . ((:) ("pos", "r", "g", "b", "a")) . map ((over each) (BS.pack . show))

-- SOPs

instance Op SOP where
  opPars (CircleSOP p a) = fromListMaybe [ ("type", ShowP <$> p)
                                         , ("arc", ShowP <$> a)
                                         ]
  opPars (NoiseSOP t) = vec3Map' "t" t
  opPars OutSOP = M.empty
  opPars Sphere = M.empty
  opPars (CHOPToSOP c a) = fromListMaybe [ ("chop", Just $ ShowP c)
                                         , ("attscope", a)
                                         ]
  opType (CircleSOP _ _) = "circleSop"
  opType (NoiseSOP _) = "noiseSop"
  opType OutSOP = "outSop"
  opType Sphere = "sphere"
  opType (CHOPToSOP _ _) = "chopToSop"

  opText _ = Nothing

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
  opPars (ConstantMAT rgb alpha) = M.union (fromListMaybe [("alpha", ShowP <$> alpha)]) $ rgbMap "color" rgb
  opType (ConstantMAT _ _) = "constMat"
  opText _ = Nothing

constantMat :: Tree MAT
constantMat = GeneratorTree (ConstantMAT emptyV3 Nothing)

-- DATs
instance Op DAT where
  opPars (Table _) = M.empty
  opPars (ChopExec chop offon won onoff woff vc ) = M.fromList [("chop", ShowP chop)
                                                               , ("offtoon", ShowP . B $ isJust offon)
                                                               , ("whileon", ShowP . B $ isJust won)
                                                               , ("ontooff", ShowP . B $ isJust onoff)
                                                               , ("whileoff", ShowP . B $ isJust woff)
                                                               , ("valuechange", ShowP . B $ isJust vc)
                                                               ]
  opType (Table _) = "table"
  opType (ChopExec _ _ _ _ _ _) = "chopExec"
  opText (Table t) = Just . BS.intercalate ("\n") . map (BS.intercalate ("\t")) . toLists $ t
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

makeChopExecFunc :: BS.ByteString -> BS.ByteString
makeChopExecFunc prog = BS.concat ["def ", prog, "(channel, sampleIndex, val, prev): \n"]

table :: Matrix BS.ByteString -> Tree DAT
table = GeneratorTree . Table

chopExec :: Tree CHOP -> Tree DAT
chopExec chop = GeneratorTree $ ChopExec (treePar chop) Nothing Nothing Nothing Nothing Nothing

cell :: (Integral a, Integral b) => (Param a, Param b) -> Tree DAT -> Param BS.ByteString
cell (x, y) t = Cell x y (treePar t)

-- COMPs
instance Op COMP where
  opType (Geo _ _ _ _) = "geo"
  opType (Camera _) = "camera"
  opType Light = "light"
  opPars (Geo t s m us) = M.unions [fromListMaybe [("material", ShowP <$> m), ("scale", ShowP <$> us)], (vec3Map' "t" t), (vec3Map' "s" s)]
  opPars (Camera t) = vec3Map' "t" t
  opPars Light = M.empty
  opText _ = Nothing

geo :: Tree SOP -> Tree COMP
geo = ComponentTree (Geo emptyV3 emptyV3 Nothing Nothing)

cam :: Tree COMP
cam = GeneratorTree (Camera emptyV3)

light :: Tree COMP
light = GeneratorTree Light
