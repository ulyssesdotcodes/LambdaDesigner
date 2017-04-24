{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Tree where

import Prelude hiding (sin, concat, floor)

import Control.Lens
import Data.ByteString.Char8
import Data.Map.Strict
import Data.Maybe

data Tree a where
  GeneratorTree :: (Op a) => a -> Tree a
  EffectTree :: (Op a) => a -> Tree a -> Tree a
  CombineTree :: (Op a) => a -> Tree a -> Tree a -> Tree a
  CompositeTree :: (Op a) => a -> [Tree a] -> Tree a

  FeedbackTree :: (Op a) => a -> Tree a -> (Tree a -> Tree a) -> (Tree a -> Tree a) -> Tree a

  ComponentTree :: (Op a, Op b) => a -> Tree b -> Tree a

  FixedTree :: (Op a) => ByteString -> Tree a -> Tree a

pars :: Lens' (Tree a) a
pars f (CombineTree t o1 o2) = fmap (\t' -> CombineTree t' o1 o2) (f t)
pars f (CompositeTree t os) = fmap (\t' -> CompositeTree t' os) (f t)
pars f (GeneratorTree a) = fmap (\a' -> GeneratorTree a') (f a)
pars f (EffectTree a aop) = fmap (\a' -> EffectTree a' aop) (f a)
pars f (FeedbackTree a b c d) = fmap (\a' -> FeedbackTree a' b c d) (f a)
pars f (ComponentTree a aop) = fmap (\a' -> ComponentTree a' aop) (f a)
pars f (FixedTree _ aop) = pars f aop

class Op a where
  opType :: a -> ByteString
  opPars :: a -> Map ByteString (Param ByteString)
  opText :: a -> Maybe ByteString

treePar :: (Op a) => Tree a -> Param (Tree a)
treePar = TreePar

data Param a where
  File :: ByteString -> Param ByteString
  TreePar :: (Op a) => Tree a -> Param (Tree a)
  F :: (Floating n, Show n) => n -> Param n
  I :: (Integral i, Show i) => i -> Param i
  S :: String -> Param ByteString
  B :: Bool -> Param Bool
  MakeFloat :: (Integral a, Floating b, Show a, Show b) => Param a -> Param b
  TreeFloat :: (Num n, Op a) => (ByteString -> ByteString) -> Param (Tree a) -> Param n
  TreeString :: (Op a) => (ByteString -> ByteString) -> Param (Tree a) -> Param ByteString
  ShowP :: Param a -> Param ByteString
  PyExpr :: (Show a) => ByteString -> Param a
  Mult :: (Num a, Show a) => Param a -> Param a -> Param a
  Add :: (Num a, Show a) => Param a -> Param a -> Param a
  Mod :: (Num a, Num b, Show a) => (ByteString -> ByteString) -> Param a -> Param b
  Mod2 :: (Num a, Num b, Num c, Show a, Show b, Show c) => (ByteString -> ByteString -> ByteString) -> Param a -> Param b -> Param c
  Cell :: (Integral a, Integral b, Op c) => Param a -> Param b -> Param (Tree c) -> Param ByteString

file :: ByteString -> Param ByteString
file = File

float :: (Floating n, Show n) => n -> Param n
float = F

int :: (Integral i, Show i) => i -> Param i
int = I

ptrue :: Param Bool
ptrue = B True

pfalse :: Param Bool
pfalse = B False

toFloat :: (Integral a, Floating b, Show a, Show b) => Param a -> Param b
toFloat = MakeFloat

seconds :: Param Float
seconds = PyExpr "absTime.seconds"

frames :: Param Int
frames = PyExpr "absTime.frame"

(!*) :: (Num a, Show a) => Param a -> Param a -> Param a
(!*) = Mult

(!+) :: (Num a, Show a) => Param a -> Param a -> Param a
(!+) = Add

sin :: (Floating a, Show a) => Param a -> Param a
sin = mathFunc "sin"

sin' :: (Floating a, Show a) => Param a -> Param a
sin' a = (float 0.5) !+ ((float 0.5) !* sin a)

floor :: (Floating a, Show a) => Param a -> Param a
floor = mathFunc "floor"

floori :: (Floating a, Integral b, Show a, Show b) => Param a -> Param b
floori = mathFunc "floor"

(!%) :: (Num a, Show a) => Param a -> Param a -> Param a
(!%) = Mod2 (\op1 op2 -> concat ["(", op1, "%", op2, ")"])

mathFunc :: (Num a, Num b, Show a) => ByteString -> Param a -> Param b
mathFunc f = Mod (\op -> concat ["math.", f, "(", op, ")"])

type Vec3 = (Maybe (Param Float), Maybe (Param Float), Maybe (Param Float))

type Vec2 = (Maybe (Param Float), Maybe (Param Float))

type IVec2 = (Maybe (Param Int), Maybe (Param Int))

type RGB = Vec3

type Dimen = IVec2

grey :: Float -> RGB
grey = (\a -> (a, a, a)) . Just . float

emptyV3 :: Vec3
emptyV3 = (Nothing, Nothing, Nothing)

emptyV2 :: Vec2
emptyV2 = (Nothing, Nothing)

emptyIV2 :: IVec2
emptyIV2 = (Nothing, Nothing)

rgbMap :: String -> RGB -> Map ByteString (Param ByteString)
rgbMap = vec3Map ("r", "g", "b")

dimenMap :: String -> Dimen-> Map ByteString (Param ByteString)
dimenMap = vec2Map ("w", "h")

vec2Map' :: String -> Vec2 -> Map ByteString (Param ByteString)
vec2Map' = vec2Map ("x", "y")

vec3Map' :: String -> Vec3 -> Map ByteString (Param ByteString)
vec3Map' = vec3Map ("x", "y", "z")

vec2Map :: (Show a) => (String, String) -> String -> (Maybe (Param a), Maybe (Param a)) -> Map ByteString (Param ByteString)
vec2Map (x, y) pre (xv, yv) = fromListMaybe [ (pack $ pre ++ x, ShowP <$> xv)
                                            , (pack $ pre ++ y, ShowP <$> yv)
                                            ]

vec3Map :: (String, String, String) -> String -> Vec3 -> Map ByteString (Param ByteString)
vec3Map (x, y, z) pre (xv, yv, zv) = fromListMaybe [ (pack $ pre ++ x, ShowP <$> xv)
                                                   , (pack $ pre ++ y, ShowP <$> yv)
                                                   , (pack $ pre ++ z, ShowP <$> zv)
                                                   ]

fromListMaybe :: (Ord k) => [(k, Maybe a)] -> Map k a
fromListMaybe = fromList . fmap (\(k, a) -> (k, fromJust a)) . Prelude.filter (isJust . snd)
