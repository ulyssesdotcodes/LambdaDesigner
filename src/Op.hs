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
  I :: (Integral i, Show i) => i -> Param i
  CHOPOpPar :: Param (Tree CHOP) -> Param BS.ByteString
  ShowP :: Param a -> Param BS.ByteString
  Seconds :: Param Float
  Mult :: Param a -> Param a -> Param Float



-- Ops

class Op a where
  opType :: a -> BS.ByteString
  opPars :: a -> M.Map BS.ByteString (Param BS.ByteString)

-- TOPs

data TOP = CHOPTo { _chopToTop :: Param (Tree CHOP) }
             | Displace
             | MovieFileIn { _movieFileInFile :: Param BS.ByteString }
             | OutTOP

data CHOP = NoiseCHOP { _translatex :: Maybe (Param Float)
                      , _translatey :: Maybe (Param Float)
                      , _translatez :: Maybe (Param Float)
                      , _roughness :: Maybe (Param Float)
                      , _noiseType :: Maybe (Param Int)
                      }

data SOP = Sphere
         | OutSOP
-- Trees

data Tree a where
  CHOPToTree :: TOP -> Tree TOP
  DisplaceTree :: TOP -> Tree TOP -> Tree TOP -> Tree TOP
  MovieFileInTree :: TOP -> Tree TOP
  OutTOPTree :: TOP -> Tree TOP -> Tree TOP

  NoiseCHOPTree :: CHOP -> Tree CHOP

  SphereTree :: SOP -> Tree SOP
  OutSOPTree :: SOP -> Tree SOP -> Tree SOP

treePars :: Lens' (Tree a) a
treePars f (CHOPToTree t) = fmap (\t' -> CHOPToTree t') (f t)
treePars f (DisplaceTree t o1 o2) = fmap (\t' -> DisplaceTree t' o1 o2) (f t)
treePars f (MovieFileInTree t) = fmap (\t' -> MovieFileInTree t') (f t)
treePars f (NoiseCHOPTree c) = fmap (\c' -> NoiseCHOPTree c') (f c)
treePars f (OutTOPTree t top) = fmap (\t' -> OutTOPTree t' top) (f t)
treePars f (SphereTree s) = fmap (\s' -> SphereTree s') (f s)
treePars f (OutSOPTree s sop) = fmap (\s' -> OutSOPTree s' sop) (f s)

makeLenses ''CHOP
makeLenses ''TOP

instance Op TOP where
  opPars (CHOPTo chop) = M.singleton (BS.pack "chop") (CHOPOpPar chop)
  opPars Displace = M.empty
  opPars (MovieFileIn file) = M.singleton (BS.pack "file") file
  opPars OutTOP = M.empty
  opType (CHOPTo _) = "chopToTop"
  opType (Displace) = "displace"
  opType (MovieFileIn _) = "movieFileIn"
  opType OutTOP = "outTop"

movieFileIn :: String -> Tree TOP
movieFileIn (BS.pack -> file) = MovieFileInTree (MovieFileIn (File file))

displace :: Tree TOP -> Tree TOP -> Tree TOP
displace = DisplaceTree Displace

chopTo :: Tree CHOP -> Tree TOP
chopTo = CHOPToTree . CHOPTo . CHOPPar

outTop :: Tree TOP -> Tree TOP
outTop = OutTOPTree OutTOP

-- CHOPs

instance Op CHOP where
  opType (NoiseCHOP _ _ _ _ _) = "noiseCHOP"
  opPars (NoiseCHOP tx ty tz r nt) = alterMap [ ("tx", ShowP <$> tx)
                                           , ("ty", ShowP <$> ty)
                                           , ("tz", ShowP <$> tz)
                                           , ("roughness", ShowP <$> r)
                                           , ("type", ShowP <$> nt)
                                           ]

alterMap :: [(BS.ByteString, Maybe (Param BS.ByteString))] -> M.Map BS.ByteString (Param BS.ByteString)
alterMap ((n, v):ps) = M.alter (const v) n $ alterMap ps
alterMap [] = M.empty

noiseCHOP :: Tree CHOP
noiseCHOP = NoiseCHOPTree (NoiseCHOP Nothing Nothing Nothing Nothing Nothing)

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
