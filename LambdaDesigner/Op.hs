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

tox :: (Op a, Op b) => String -> (COMP -> COMP) -> [(ByteString, Tree ByteString)] -> Tree a -> Tree b
tox t fs ps = Comp (bcomppars & (baseCOMPexternaltox ?~ str t) & fs & (compCommands .~ [Pulse "reinitnet" "1" 2])) ps ([] :: [Tree TOP]) ([] :: [Tree CHOP]) ([] :: [Tree DAT]) . (:[])

tox0 :: (Op a) => String -> (COMP -> COMP) -> [(ByteString, Tree ByteString)] -> Tree a
tox0 t fs ps = Comp (bcomppars & (baseCOMPexternaltox ?~ (str t)) & fs & (compCommands .~ [Pulse "reinitnet" "1" 2])) ps ([] :: [Tree TOP]) ([] :: [Tree CHOP]) ([] :: [Tree DAT]) ([] :: [Tree MAT])

tox2 :: (Op a, Op b, Op c) => String -> (COMP -> COMP) -> [(ByteString, Tree ByteString)] -> Tree a -> Tree b -> Tree c
tox2 t fs ps a b = Comp (bcomppars & (baseCOMPexternaltox ?~ str t) & fs & (compCommands .~ [Pulse "reinitnet" "1" 2])) ps ([] :: [Tree TOP]) ([] :: [Tree CHOP]) ([a]) ([b])