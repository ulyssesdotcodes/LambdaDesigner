{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Tree where

import Prelude hiding (sin)

import Control.Lens
import Data.ByteString.Char8
import Data.Map.Strict
import Data.Maybe

data Tree a where
  GeneratorTree :: a -> Tree a
  EffectTree :: a -> Tree a -> Tree a
  CompositeTree :: a -> Tree a -> Tree a -> Tree a

  FeedbackTree :: a -> Tree a -> (Tree a -> Tree a) -> (Tree a -> Tree a -> Tree a) -> Tree a

  ComponentTree :: (Op b) => a -> Tree b -> Tree a

pars :: Lens' (Tree a) a
pars f (CompositeTree t o1 o2) = fmap (\t' -> CompositeTree t' o1 o2) (f t)
pars f (GeneratorTree a) = fmap (\a' -> GeneratorTree a') (f a)
pars f (EffectTree a aop) = fmap (\a' -> EffectTree a' aop) (f a)
pars f (FeedbackTree a b c d) = fmap (\a' -> FeedbackTree a' b c d) (f a)
pars f (ComponentTree a aop) = fmap (\a' -> ComponentTree a' aop) (f a)

class Op a where
  opType :: a -> ByteString
  opPars :: a -> Map ByteString (Param ByteString)

treePar :: (Op a) => Tree a -> Param (Tree a)
treePar = TreePar

data Param a where
  File :: ByteString -> Param ByteString
  TreePar :: (Op a) => Tree a -> Param (Tree a)
  F :: (Floating n, Show n) => n -> Param n
  I :: (Integral i, Show i) => i -> Param i
  S :: String -> Param ByteString
  TreeFloat :: (Floating n) => (ByteString -> ByteString) -> Param (Tree a) -> Param n
  ShowP :: Param a -> Param ByteString
  Seconds :: Param Float
  Mult :: (Floating a) => Param a -> Param a -> Param Float
  Add :: (Floating a) => Param a -> Param a -> Param Float
  Sin :: (Floating a) => Param a -> Param Float

file :: ByteString -> Param ByteString
file = File

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

data RGB = RGB { _r :: Maybe (Param Float)
               , _g :: Maybe (Param Float)
               , _b :: Maybe (Param Float)
               }

makeLenses ''Vec2
makeLenses ''Vec3

rgbMap :: String -> RGB -> Map ByteString (Param ByteString)
rgbMap pre (RGB r g b) = fromListMaybe [ (pack $ pre ++ "r", ShowP <$> r)
                                       , (pack $ pre ++ "g", ShowP <$> g)
                                       , (pack $ pre ++ "b", ShowP <$> b)
                                       ]

emptyRgb :: RGB
emptyRgb = RGB Nothing Nothing Nothing

grey :: Float -> RGB
grey = (\a -> RGB a a a) . Just . float

vec2Map :: String -> Vec2 -> Map ByteString (Param ByteString)
vec2Map pre (Vec2 x y) = fromListMaybe [ (pack $ pre ++ "x", ShowP <$> x)
                                       , (pack $ pre ++ "y", ShowP <$> y)
                                       ]

vec3Map :: String -> Vec3 -> Map ByteString (Param ByteString)
vec3Map pre (Vec3 xy z) = union (vec2Map pre xy) $ fromListMaybe [ (pack $ pre ++ "z", ShowP <$> z) ]

emptyV3 :: Vec3
emptyV3 = Vec3 emptyV2 Nothing

emptyV2 :: Vec2
emptyV2 = Vec2 Nothing Nothing

fromListMaybe :: (Ord k) => [(k, Maybe a)] -> Map k a
fromListMaybe = fromList . fmap (\(k, a) -> (k, fromJust a)) . Prelude.filter (isJust . snd)
