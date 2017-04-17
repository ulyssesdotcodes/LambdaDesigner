{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Op where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M

import Control.Lens

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
  F :: (Floating n, Show n) => n -> Param n
  CHOPOpPar :: Param (Tree CHOP) -> Param BS.ByteString
  ShowF :: (Floating n) => Param n -> Param BS.ByteString
  Seconds :: Param Float
  Mult :: (Floating n, Floating b) => Param n -> Param b -> Param Float

-- Ops

class Op a where
  opType :: a -> BS.ByteString
  opPars :: a -> M.Map BS.ByteString (Param BS.ByteString)

-- TOPs

data TOP = CHOPTo { _chopToTop :: Param (Tree CHOP) }
             | Displace
             | MovieFileIn { _movieFileInFile :: Param BS.ByteString }

data CHOP = NoiseCHOP { _translatex :: Maybe (Param Float)
                          }

-- Trees

data Tree a where
  CHOPToTree :: TOP -> Tree TOP
  DisplaceTree :: TOP -> Tree TOP -> Tree TOP -> Tree TOP
  MovieFileInTree :: TOP -> Tree TOP

  NoiseCHOPTree :: CHOP -> Tree CHOP

treeLens :: Lens' (Tree a) a
treeLens f (CHOPToTree t) = fmap (\t' -> CHOPToTree t') (f t)
treeLens f (DisplaceTree t o1 o2) = fmap (\t' -> DisplaceTree t' o1 o2) (f t)
treeLens f (MovieFileInTree t) = fmap (\t' -> MovieFileInTree t') (f t)
treeLens f (NoiseCHOPTree c) = fmap (\c' -> NoiseCHOPTree c') (f c)

makeLenses ''CHOP
makeLenses ''TOP

instance Op TOP where
  opPars (CHOPTo chop) = M.singleton (BS.pack "chop") (CHOPOpPar chop)
  opPars Displace = M.empty
  opPars (MovieFileIn file) = M.singleton (BS.pack "file") file
  opType (CHOPTo _) = "chopToTop"
  opType (Displace) = "displace"
  opType (MovieFileIn _) = "movieFileIn"

movieFileIn :: String -> Tree TOP
movieFileIn (BS.pack -> file) = MovieFileInTree (MovieFileIn (File file))

displace :: Tree TOP -> Tree TOP -> Tree TOP
displace = DisplaceTree Displace

chopTo :: Tree CHOP -> Tree TOP
chopTo = CHOPToTree . CHOPTo . CHOPPar

-- CHOPs

instance Op CHOP where
  opPars (NoiseCHOP tx) = M.alter (const (ShowF <$> tx)) "tx" M.empty
  opType (NoiseCHOP _) = "noiseCHOP"

noiseCHOP :: Tree CHOP
noiseCHOP = NoiseCHOPTree (NoiseCHOP Nothing)
