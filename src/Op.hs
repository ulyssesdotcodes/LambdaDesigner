{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Op where

import Prelude hiding (sin)

import Tree

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M

import Control.Lens

data CHOP = NoiseCHOP { _noiseTranslate :: Vec3
                      , _noiseRoughness :: Maybe (Param Float)
                      , _noiseType :: Maybe (Param Int)
                      }
          | SOPToCHOP { _sopToChopSop :: Param (Tree SOP) }


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
           | CompositeTOP { _operand :: Param Int }
           | CircleTOP
           | LevelTOP { _opacity :: Maybe (Param Float)
                      }

data SOP = Sphere
         | OutSOP
         | CircleSOP { _circType :: Maybe (Param Int)
                     , _circArc :: Maybe (Param Int)
                     }
         | NoiseSOP
         | CHOPToSOP { _chopToSopChop :: Param (Tree CHOP)
                     , _chopToSopAttrScope :: Maybe (Param BS.ByteString)
                     }

data MAT = ConstantMAT { _constColor :: RGB
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

-- Chops

instance Op CHOP where
  opPars (NoiseCHOP t r nt) = M.union (fromListMaybe [ ("roughness", ShowP <$> r) , ("type", ShowP <$> nt)]) $ vec3Map "t" t
  opPars (SOPToCHOP s) = M.singleton "sop" $ ShowP s
  opType (NoiseCHOP _ _ _) = "noiseCHOP"
  opType (SOPToCHOP _) = "sopToChop"

noiseCHOP :: Tree CHOP
noiseCHOP = GeneratorTree (NoiseCHOP emptyV3 Nothing Nothing)

chopChan :: Int -> Tree CHOP -> Param Float
chopChan i = TreeFloat (\opstring -> BS.append opstring (BS.pack $ "[" ++ show i ++ "]")) . TreePar

chopChan0 :: Tree CHOP -> Param Float
chopChan0 = chopChan 0

sopToChop :: Tree SOP -> Tree CHOP
sopToChop = GeneratorTree . SOPToCHOP . TreePar

-- Tops

instance Op TOP where
  opPars (CHOPToTOP chop) = M.singleton (BS.pack "chop") (ShowP chop)
  opPars (CompositeTOP op) = fromListMaybe [("operand", Just $ ShowP op)]
  opPars (FeedbackTOP) = M.empty
  opPars (MovieFileIn file) = M.singleton (BS.pack "file") file
  opPars (Render geo cam light) = fromListMaybe [ ("geometry", Just $ ShowP geo)
                                                , ("camera", Just $ ShowP cam)
                                                , ("lights", ShowP <$> light)]
  opPars (Transform t s) = M.union (vec2Map "t" t) (vec2Map "s" s)
  opPars (LevelTOP o) = fromListMaybe [("opacity", ShowP <$> o)]
  opPars CircleTOP = M.empty
  opPars Displace = M.empty
  opPars OutTOP = M.empty
  opPars NullTOP = M.empty

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

movieFileIn :: String -> Tree TOP
movieFileIn (BS.pack -> file) = GeneratorTree (MovieFileIn (File file))

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

-- SOPs

instance Op SOP where
  opPars (CircleSOP p a) = fromListMaybe [ ("type", ShowP <$> p)
                                         , ("arc", ShowP <$> a)
                                         ]
  opPars (NoiseSOP) = M.empty
  opPars OutSOP = M.empty
  opPars Sphere = M.empty
  opPars (CHOPToSOP c a) = fromListMaybe [ ("chop", Just $ ShowP c)
                                         , ("attscope", a)
                                         ]
  opType (CircleSOP _ _) = "circleSop"
  opType (NoiseSOP) = "noiseSop"
  opType OutSOP = "outSop"
  opType Sphere = "sphere"
  opType (CHOPToSOP _ _) = "chopToSop"

circleSop :: Tree SOP
circleSop = GeneratorTree $ CircleSOP Nothing Nothing

noiseSop :: Tree SOP -> Tree SOP
noiseSop = EffectTree $ NoiseSOP

sphere :: Tree SOP
sphere = GeneratorTree Sphere

outSop :: Tree SOP -> Tree SOP
outSop = EffectTree OutSOP

chopToSop :: Tree SOP -> Tree CHOP -> Tree SOP
chopToSop s c = EffectTree (CHOPToSOP (treePar c) Nothing) s

-- MATs

instance Op MAT where
  opPars (ConstantMAT rgb) = rgbMap "color" rgb
  opType (ConstantMAT _) = "constMat"

constantMat :: Tree MAT
constantMat = GeneratorTree (ConstantMAT emptyRgb)

-- COMPs
instance Op COMP where
  opType (Geo _ _ _ _) = "geo"
  opType (Camera _) = "camera"
  opType Light = "light"
  opPars (Geo t s m us) = M.unions [fromListMaybe [("material", ShowP <$> m), ("scale", ShowP <$> us)], (vec3Map "t" t), (vec3Map "s" s)]
  opPars (Camera t) = vec3Map "t" t
  opPars Light = M.empty

geo :: Tree SOP -> Tree COMP
geo = ComponentTree (Geo emptyV3 emptyV3 Nothing Nothing)

cam :: Tree COMP
cam = GeneratorTree (Camera emptyV3)

light :: Tree COMP
light = GeneratorTree Light
