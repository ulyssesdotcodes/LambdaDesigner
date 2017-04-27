{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Tree where

import Prelude hiding (sin, concat)

import Control.Applicative
import Data.ByteString.Char8
import Data.Map.Strict
import Data.Maybe

data TDFloat
data TDInt
data TDString
class TDNum a
instance TDNum TDFloat
instance TDNum TDInt

data TDType a where
  Empty :: TDType a
  F :: (Floating n, Show n) => n -> TDType n
  I :: (Integral i, Show i) => i -> TDType i
  S :: String -> TDType ByteString
  B :: Bool -> TDType Bool
  BS :: ByteString -> TDType TDString
  TDF :: (Floating n) => TDType n -> TDType TDFloat
  TDI :: (Integral n) => TDType n -> TDType TDInt
  TDFloatExpr :: (a -> ByteString) -> a -> TDType TDFloat
  TDIntExpr :: (a -> ByteString) -> a -> TDType TDInt
  Mod :: (TDNum a, TDNum b) => (ByteString -> ByteString) -> TDType a -> TDType b
  Mod2 :: (TDNum a, TDNum b, TDNum c) => ((ByteString -> ByteString -> ByteString)) -> TDType a -> TDType b -> TDType c
  Resolve :: TDType a -> TDType ByteString

parse :: TDType a -> Maybe ByteString
parse (F i) = Just $ pack $ show i
parse (I i) = Just $ pack $ show i
parse (S s) = Just $ pack s
parse (B b) = Just $ pack . show $ if b then 1 else 0
parse (BS bs) = Just bs
parse (TDF n) = parse n
parse (TDI n) = parse n
parse (TDFloatExpr f a) = Just $ f a
parse (TDIntExpr f a) = Just $ f a
parse (Mod f a) = f <$> parse a
parse (Mod2 f a b) = liftA2 f (parse a) (parse b)
parse (Resolve a) = parse a
parse Empty = Nothing

float :: (Floating n, Show n) => n -> TDType n
float = F

int :: (Integral i, Show i) => i -> TDType i
int = I

ptrue :: TDType Bool
ptrue = B True

pfalse :: TDType Bool
pfalse = B False

pmax :: (TDNum n) => TDType n -> TDType n -> TDType n
pmax = Mod2 (\num n -> concat ["max(", n, ",", num, ")"])

seconds :: TDType TDFloat
seconds = TDFloatExpr pack "absTime.seconds"

frames :: TDType TDInt
frames = TDIntExpr pack "absTime.frame"

(!*) :: (TDNum n) => TDType n -> TDType n -> TDType n
(!*) = Mod2 (\op1 op2 -> concat ["(", op1, "*", op2, ")"])

(!+) :: (TDNum n) => TDType n -> TDType n -> TDType n
(!+) = Mod2 (\op1 op2 -> concat ["(", op1, "+", op2, ")"])

(!%) :: (TDNum n) => TDType n -> TDType n -> TDType n
(!%) = Mod2 (\op1 op2 -> concat ["(", op1, "%", op2, ")"])

sin :: TDType TDFloat -> TDType TDFloat
sin = mathFunc "sin"

sin' :: TDType TDFloat -> TDType TDFloat
sin' a = (TDF $ float 0.5) !+ ((TDF $ float 0.5) !* sin a)

floorp :: TDType TDFloat -> TDType TDInt
floorp = mathFunc "floor"

mathFunc :: (TDNum a, TDNum b) => ByteString -> TDType a -> TDType b
mathFunc f = Mod (\op -> concat ["math.", f, "(", op, ")"])

type Vec3 = ((TDType TDFloat),  (TDType TDFloat),  (TDType TDFloat))

type Vec2 = ((TDType TDFloat),  (TDType TDFloat))

type IVec2 = ((TDType TDInt),  (TDType TDInt))

type RGB = Vec3

type Dimen = IVec2

grey :: Float -> RGB
grey = (\a -> (a, a, a)) . TDF . float

emptyV3 :: Vec3
emptyV3 = (Empty, Empty, Empty)

emptyV2 :: Vec2
emptyV2 = (Empty, Empty)

emptyIV2 :: IVec2
emptyIV2 = (Empty, Empty)

rgbMap :: String -> RGB -> Map ByteString (TDType ByteString)
rgbMap = vec3Map ("r", "g", "b")

dimenMap :: String -> Dimen-> Map ByteString (TDType ByteString)
dimenMap = vec2Map ("w", "h")

vec2Map' :: String -> Vec2 -> Map ByteString (TDType ByteString)
vec2Map' = vec2Map ("x", "y")

vec3Map' :: String -> Vec3 -> Map ByteString (TDType ByteString)
vec3Map' = vec3Map ("x", "y", "z")

vec3Map :: (TDNum a) => (ByteString, ByteString, ByteString) -> String -> (TDType a, TDType a, TDType a) -> Map ByteString (TDType ByteString)
vec3Map (x, y, z) n (xv, yv, zv) = fromList [(x, Resolve xv),  (y, Resolve yv), (z, Resolve zv)]

vec2Map :: (TDNum a) => (ByteString, ByteString) -> String -> (TDType a, TDType a) -> Map ByteString (TDType ByteString)
vec2Map (x, y) n (xv, yv) = fromList [(x, Resolve xv),  (y, Resolve yv)]
