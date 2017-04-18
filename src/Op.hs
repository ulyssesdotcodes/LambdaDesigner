{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Op where

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
  CHOPOpPar :: Param (Tree CHOP) -> Param BS.ByteString
  ShowP :: Param a -> Param BS.ByteString
  Seconds :: Param Float
  Mult :: (Floating a) => Param a -> Param a -> Param Float
  Add :: (Floating a) => Param a -> Param a -> Param Float
  Sin :: (Floating a) => Param a -> Param Float
  Sin' :: (Floating a) => Param a -> Param Float

data Vec3 = Vec3 { _x :: Maybe (Param Float)
                 , _y :: Maybe (Param Float)
                 , _z :: Maybe (Param Float)
                 }

vec3Map :: String -> Vec3 -> M.Map BS.ByteString (Param BS.ByteString)
vec3Map pre (Vec3 x y z) = fromListMaybe [ (BS.pack $ pre ++ "x", ShowP <$> x)
                                         , (BS.pack $ pre ++ "y", ShowP <$> y)
                                         , (BS.pack $ pre ++ "z", ShowP <$> z)
                                         ]
emptyV3 :: Vec3
emptyV3 = Vec3 Nothing Nothing Nothing

fromListMaybe :: (Ord k) => [(k, Maybe a)] -> M.Map k a
fromListMaybe = M.fromList . fmap (\(k, a) -> (k, fromJust a)) . filter (isJust . snd)

-- Ops

class Op a where
  opType :: a -> BS.ByteString
  opPars :: a -> M.Map BS.ByteString (Param BS.ByteString)

-- TOPs

data TOP = CHOPTo { _chopToTop :: Param (Tree CHOP) }
             | Displace
             | MovieFileIn { _movieFileInFile :: Param BS.ByteString }
             | OutTOP
             | Render { _renderGeo :: Param (Tree COMP)
                      , _renderCamera :: Param (Tree COMP)
                      , _renderLight :: Maybe (Param (Tree COMP))
                      }

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
  CHOPToTree :: TOP -> Tree TOP
  DisplaceTree :: TOP -> Tree TOP -> Tree TOP -> Tree TOP
  MovieFileInTree :: TOP -> Tree TOP
  OutTOPTree :: TOP -> Tree TOP -> Tree TOP
  RenderTree :: TOP -> Tree TOP

  NoiseCHOPTree :: CHOP -> Tree CHOP

  SphereTree :: SOP -> Tree SOP
  OutSOPTree :: SOP -> Tree SOP -> Tree SOP

  GeoTree :: COMP -> Tree SOP -> Tree COMP
  CameraTree :: COMP -> Tree COMP
  LightTree :: COMP -> Tree COMP

treePars :: Lens' (Tree a) a
treePars f (CHOPToTree t) = fmap (\t' -> CHOPToTree t') (f t)
treePars f (DisplaceTree t o1 o2) = fmap (\t' -> DisplaceTree t' o1 o2) (f t)
treePars f (MovieFileInTree t) = fmap (\t' -> MovieFileInTree t') (f t)
treePars f (OutTOPTree t top) = fmap (\t' -> OutTOPTree t' top) (f t)
treePars f (RenderTree t) = fmap (\t' -> RenderTree t') (f t)
treePars f (NoiseCHOPTree c) = fmap (\c' -> NoiseCHOPTree c') (f c)
treePars f (SphereTree s) = fmap (\s' -> SphereTree s') (f s)
treePars f (OutSOPTree s sop) = fmap (\s' -> OutSOPTree s' sop) (f s)
treePars f (GeoTree c sop) = fmap (\c' -> GeoTree c' sop) (f c)
treePars f (CameraTree c) = fmap (\c' -> CameraTree c') (f c)
treePars f (LightTree c) = fmap (\c' -> LightTree c') (f c)

makeLenses ''CHOP
makeLenses ''TOP
makeLenses ''SOP
makeLenses ''COMP

makeLenses ''Vec3

instance Op TOP where
  opPars (CHOPTo chop) = M.singleton (BS.pack "chop") (CHOPOpPar chop)
  opPars Displace = M.empty
  opPars (MovieFileIn file) = M.singleton (BS.pack "file") file
  opPars OutTOP = M.empty
  opPars (Render geo cam light) = fromListMaybe [ ("geometry", Just $ ShowP geo)
                                                , ("camera", Just $ ShowP cam)
                                                , ("lights", ShowP <$> light)
                                                ]
  opType (CHOPTo _) = "chopToTop"
  opType (Displace) = "displace"
  opType (MovieFileIn _) = "movieFileIn"
  opType OutTOP = "outTop"
  opType (Render _ _ _) = "render"

movieFileIn :: String -> Tree TOP
movieFileIn (BS.pack -> file) = MovieFileInTree (MovieFileIn (File file))

displace :: Tree TOP -> Tree TOP -> Tree TOP
displace = DisplaceTree Displace

chopTo :: Tree CHOP -> Tree TOP
chopTo = CHOPToTree . CHOPTo . CHOPPar

outTop :: Tree TOP -> Tree TOP
outTop = OutTOPTree OutTOP

render :: Tree COMP -> Tree COMP -> Tree TOP
render geo cam = RenderTree (Render (COMPPar geo) (COMPPar cam) Nothing)

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
noiseCHOP = NoiseCHOPTree (NoiseCHOP emptyV3 Nothing Nothing)

-- SOPs

instance Op SOP where
  opType Sphere = "sphere"
  opType OutSOP = "outSop"
  opPars Sphere = M.empty
  opPars OutSOP = M.empty

sphere :: Tree SOP
sphere = SphereTree Sphere

outSop :: Tree SOP -> Tree SOP
outSop = OutSOPTree OutSOP

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
cam = CameraTree (Camera emptyV3)

light :: Tree COMP
light = LightTree Light
