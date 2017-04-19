{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Op where

import Prelude hiding (sin)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M

import Control.Lens
import Data.Maybe

type Address = BS.ByteString
type OpType = BS.ByteString
type ParName = BS.ByteString
type ParValue = BS.ByteString

-- Parameters

class Numeric a where
  numericValue :: a -> BS.ByteString

data Param a where
  File :: BS.ByteString -> Param BS.ByteString
  CHOPPar :: Tree CHOP -> Param (Tree CHOP)
  COMPPar :: Tree COMP -> Param (Tree COMP)
  F :: (Floating n, Show n) => n -> Param n
  I :: (Integral i, Show i) => i -> Param i
  CHOPChan :: Tree CHOP -> Int -> Param Float
  ShowP :: Param a -> Param BS.ByteString
  Seconds :: Param Float
  Mult :: (Floating a) => Param a -> Param a -> Param Float
  Add :: (Floating a) => Param a -> Param a -> Param Float
  Sin :: (Floating a) => Param a -> Param Float

file :: BS.ByteString -> Param BS.ByteString
file = File

chopPar :: Tree CHOP -> Param (Tree CHOP)
chopPar = CHOPPar

chopChan :: Tree CHOP -> Int -> Param Float
chopChan = CHOPChan

chopChan0 :: Tree CHOP -> Param Float
chopChan0 = flip chopChan 0

compPar :: Tree COMP -> Param (Tree COMP)
compPar = COMPPar

float :: (Floating n, Show n) => n -> Param n
float = F

int :: (Integral i, Show i) => i -> Param i
int = I

seconds :: Param Float
seconds = Seconds

(!*) :: (Floating a) => Param a -> Param a -> Param Float
(!*) = Mult

(!+) :: (Floating a) => Param a -> Param a -> Param Float
(!+) = Add

sin :: (Floating a) => Param a -> Param Float
sin = Sin

sin' :: (Floating a) => Param a -> Param Float
sin' a = (float 0.5) !+ ((float 0.5) !* sin a)

data Vec3 = Vec3 { _xy :: Vec2
                 , _z :: Maybe (Param Float)
                 }

data Vec2 = Vec2 { _x :: Maybe (Param Float)
                 , _y :: Maybe (Param Float)
                 }

vec2Map :: String -> Vec2 -> M.Map BS.ByteString (Param BS.ByteString)
vec2Map pre (Vec2 x y) = fromListMaybe [ (BS.pack $ pre ++ "x", ShowP <$> x)
                                       , (BS.pack $ pre ++ "y", ShowP <$> y)
                                       ]

vec3Map :: String -> Vec3 -> M.Map BS.ByteString (Param BS.ByteString)
vec3Map pre (Vec3 xy z) = M.union (vec2Map pre xy) $ fromListMaybe [ (BS.pack $ pre ++ "z", ShowP <$> z) ]

emptyV3 :: Vec3
emptyV3 = Vec3 emptyV2 Nothing

emptyV2 :: Vec2
emptyV2 = Vec2 Nothing Nothing


fromListMaybe :: (Ord k) => [(k, Maybe a)] -> M.Map k a
fromListMaybe = M.fromList . fmap (\(k, a) -> (k, fromJust a)) . filter (isJust . snd)

-- Ops

class Op a where
  opType :: a -> BS.ByteString
  opPars :: a -> M.Map BS.ByteString (Param BS.ByteString)

-- Ops (records are parameters)

data TOP = CHOPTo { _chopToTop :: Param (Tree CHOP) }
             | Displace
             | MovieFileIn { _movieFileInFile :: Param BS.ByteString }
             | OutTOP
             | Render { _renderGeo :: Param (Tree COMP)
                      , _renderCamera :: Param (Tree COMP)
                      , _renderLight :: Maybe (Param (Tree COMP))
                      }
             | FeedbackTOP
             | Transform { _transformTranslate :: Vec2
                         , _transformScale :: Vec2
                         }
             | CompositeTOP { _operand :: Param Int }
             | Circle

data CHOP = NoiseCHOP { _noiseTranslate :: Vec3
                      , _noiseRoughness :: Maybe (Param Float)
                      , _noiseType :: Maybe (Param Int)
                      }

data SOP = Sphere
         | OutSOP

data COMP = Geo { _geoTranslate :: Vec3
                , _geoScale :: Vec3
                }
          | Camera { _camTranslate :: Vec3
                   }
          | Light

-- Trees

data Tree a where
  GeneratorTree :: a -> Tree a
  EffectTree :: a -> Tree a -> Tree a
  CompositeTree :: a -> Tree a -> Tree a -> Tree a

  FeedbackTopTree :: TOP -> Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP -> Tree TOP) -> Tree TOP

  GeoTree :: COMP -> Tree SOP -> Tree COMP

pars :: Lens' (Tree a) a
pars f (CompositeTree t o1 o2) = fmap (\t' -> CompositeTree t' o1 o2) (f t)
pars f (GeneratorTree a) = fmap (\a' -> GeneratorTree a') (f a)
pars f (EffectTree a aop) = fmap (\a' -> EffectTree a' aop) (f a)
pars f (GeoTree c sop) = fmap (\c' -> GeoTree c' sop) (f c)

makeLenses ''CHOP
makeLenses ''TOP
makeLenses ''SOP
makeLenses ''COMP

makeLenses ''Vec2
makeLenses ''Vec3

-- Tops

instance Op TOP where
  opPars (CHOPTo chop) = M.singleton (BS.pack "chop") (ShowP chop)
  opPars Displace = M.empty
  opPars (MovieFileIn file) = M.singleton (BS.pack "file") file
  opPars OutTOP = M.empty
  opPars (Render geo cam light) = fromListMaybe [ ("geometry", Just $ ShowP geo)
                                                , ("camera", Just $ ShowP cam)
                                                , ("lights", ShowP <$> light)
                                                ]
  opPars (Transform t s) = M.union (vec2Map "t" t) (vec2Map "s" s)
  opPars (FeedbackTOP) = M.empty
  opPars (CompositeTOP op) = fromListMaybe [("operand", Just $ ShowP op)]
  opPars Circle = M.empty
  opType (CHOPTo _) = "chopToTop"
  opType (Displace) = "displace"
  opType (MovieFileIn _) = "movieFileIn"
  opType OutTOP = "outTop"
  opType (Render _ _ _) = "render"
  opType FeedbackTOP = "feedbackTop"
  opType Circle = "circle"
  opType (CompositeTOP _) = "compositeTop"
  opType (Transform _ _) = "transform"

movieFileIn :: String -> Tree TOP
movieFileIn (BS.pack -> file) = GeneratorTree (MovieFileIn (File file))

displace :: Tree TOP -> Tree TOP -> Tree TOP
displace = CompositeTree Displace

chopTo :: Tree CHOP -> Tree TOP
chopTo = GeneratorTree . CHOPTo . CHOPPar

outTop :: Tree TOP -> Tree TOP
outTop = EffectTree OutTOP

render :: Tree COMP -> Tree COMP -> Tree TOP
render geo cam = GeneratorTree (Render (COMPPar geo) (COMPPar cam) Nothing)

compTop :: Int -> Tree TOP -> Tree TOP -> Tree TOP
compTop op = CompositeTree (CompositeTOP $ int op)

feedback :: Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP -> Tree TOP) -> Tree TOP
feedback = FeedbackTopTree $ FeedbackTOP

circle :: Tree TOP
circle = GeneratorTree Circle

transformTop :: Tree TOP -> Tree TOP
transformTop = EffectTree (Transform emptyV2 emptyV2)

-- CHOPs

instance Op CHOP where
  opType (NoiseCHOP _ _ _) = "noiseCHOP"
  opPars (NoiseCHOP t r nt) = M.union (fromListMaybe [ ("roughness", ShowP <$> r)
                                                      , ("type", ShowP <$> nt)
                                                      ]) $ vec3Map "t" t

alterMap :: [(BS.ByteString, Maybe (Param BS.ByteString))] -> M.Map BS.ByteString (Param BS.ByteString) -> M.Map BS.ByteString (Param BS.ByteString)
alterMap ((n, v):ps) base = M.alter (const v) n $ alterMap ps base
alterMap [] base = base

noiseCHOP :: Tree CHOP
-- noiseCHOP = NoiseCHOPTree (NoiseCHOP emptyV3 Nothing Nothing)
noiseCHOP = GeneratorTree (NoiseCHOP emptyV3 Nothing Nothing)

-- SOPs

instance Op SOP where
  opType Sphere = "sphere"
  opType OutSOP = "outSop"
  opPars Sphere = M.empty
  opPars OutSOP = M.empty

sphere :: Tree SOP
-- sphere = SphereTree Sphere
sphere = GeneratorTree Sphere

outSop :: Tree SOP -> Tree SOP
outSop = EffectTree OutSOP

-- COMPs
instance Op COMP where
  opType (Geo _ _) = "geo"
  opType (Camera _) = "camera"
  opType Light = "light"
  opPars (Geo t s) = M.union (vec3Map "t" t) (vec3Map "s" s)
  opPars (Camera t) = vec3Map "t" t
  opPars Light = M.empty

geo :: Tree SOP -> Tree COMP
geo = GeoTree (Geo emptyV3 emptyV3)

cam :: Tree COMP
cam = GeneratorTree (Camera emptyV3)

light :: Tree COMP
light = GeneratorTree Light
