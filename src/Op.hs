{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Op where

import Prelude hiding (sin)

-- import Tree

import Control.Lens
import Data.Bool
import Data.Matrix
import Data.Maybe
import Control.Monad.Trans.State.Lazy
import Data.Typeable

import Data.ByteString.Char8 as BS
import Data.List as L
import Data.Map.Strict as M
import Data.Trie as T

data CommandType = Pulse ByteString deriving Eq

class Op a where
  connections :: a -> [Tree a]
  connections _ = []
  pars :: a -> [(ByteString, Tree ByteString)]
  pars _ = []
  text :: a -> Maybe ByteString
  text _ = Nothing
  opType :: a -> ByteString
  opType _ = ""
  commands :: a -> [CommandType]
  commands _ = []

data CHOP = ConstantCHOP { _values :: [Tree Float]
                        }
          | Count { _chopIns :: [Tree CHOP]
                  , _countReset :: Maybe (Tree CHOP)
                  , _countThresh :: Maybe (Tree Float)
                  , _countLimType :: Maybe (Tree Int)
                  , _countLimMin :: Maybe (Tree Float)
                  , _countLimMax :: Maybe (Tree Float)
                  }
          | Fan { _fanOp :: Maybe (Tree Int)
                , _fanOffNeg :: Maybe (Tree Bool)
                , _chopIns :: [Tree CHOP]
                }
          | FeedbackCHOP { _chopIns :: [Tree CHOP]
                         }
          | Hold { _chopIns :: [Tree CHOP]
                 }
          | Logic { _logicPreop :: Maybe (Tree Int)
                  , _logicConvert :: Maybe (Tree Int)
                  , _chopIns :: [Tree CHOP]
                  }
          | Math { _mathAdd :: Maybe (Tree Float)
                 , _mathCombChops :: Maybe (Tree Int)
                 , _chopIns :: [Tree CHOP]
                 }
          | MergeCHOP { _chopIns :: [Tree CHOP] }
          | NoiseCHOP { _chopTimeSlice :: Maybe (Tree Bool)
                      , _noiseCTranslate :: Vec3
                      , _noiseCRoughness :: Maybe (Tree Float)
                      , _noiseCType :: Maybe (Tree Int)
                      , _noiseCPeriod :: Maybe (Tree Float)
                      }
          | SelectCHOP { _selectCChop :: Maybe (Tree CHOP)
                       }
          | SOPToCHOP { _sopToChopSop :: Tree SOP }

data DAT = Table { _tableText :: Matrix ByteString
                 }
         | ChopExec { _chopExecChop :: Tree CHOP
                    , _ceOffToOn :: Maybe BS.ByteString
                    , _ceWhileOn :: Maybe BS.ByteString
                    , _ceOnToOff :: Maybe BS.ByteString
                    , _ceWhileOff :: Maybe BS.ByteString
                    , _ceValueChange :: Maybe BS.ByteString
                    }
         | TextDAT { _textBlob :: Maybe BS.ByteString
                   , _textFile :: Maybe (Tree BS.ByteString)
                   }
         | TCPIPDAT { _tcpipMode :: Maybe (Tree Int)
                    , _tcpipCallbacks :: Tree DAT
                    , _tcpipCallbackFormat :: Maybe (Tree Int)
                    }

data SOP = Sphere
         | OutSOP { _sopIns :: [Tree SOP]
                  }
         | CircleSOP { _circType :: Maybe (Tree Int)
                     , _circArc :: Maybe (Tree Int)
                     }
         | NoiseSOP { _noiseSTranslate :: Vec3
                    , _sopIns :: [Tree SOP]
                    }
         | CHOPToSOP { _chopToSopChop :: Tree CHOP
                     , _chopToSopAttrScope :: Maybe (Tree BS.ByteString)
                     }

data TOP = CHOPToTOP { _chopToTopChop :: Tree CHOP }
           | Displace { _topIns :: [Tree TOP] }
           | MovieFileIn { _movieFileInFile :: Tree BS.ByteString
                         , _moviePlayMode :: Maybe (Tree Int)
                         , _movieIndex :: Maybe (Tree Int)
                         }
           | OutTOP { _topIns :: [Tree TOP] }
           | NullTOP { _topIns :: [Tree TOP]}
           -- | Render { _renderGeo :: Tree (Tree COMP)
           --         , _renderCamera :: Tree (Tree COMP)
           --         , _renderLight :: Tree (Tree COMP)
           --         }
           | CircleTOP
           | FeedbackTOP { _fbTop :: Maybe (Tree TOP)
                         }
           | CompositeTOP { _compTOperand :: Tree Int
                          , _topIns :: [Tree TOP]
                          }
           | LevelTOP { _levelOpacity :: Maybe (Tree Float)
                      , _topIns :: [Tree TOP]
                      }
           | NoiseTOP { _noiseTMonochrome :: Maybe (Tree Bool)
                      , _noiseTResolution :: IVec2
                      , _noiseTTranslate :: Vec3
                      }
           | Ramp { _rampType :: Maybe (Tree Int)
                  , _rampPhase :: Maybe (Tree Float)
                  , _rampResolution :: IVec2
                  , _rampValues :: Tree DAT
                  }
           | SwitchTOP { _switchTIndex :: Tree Float
                       , _topIns :: [Tree TOP]
                       }
           | SelectTOP { _selectTTop :: Maybe (Tree TOP)
                       }
           | Transform { _transformTranslate :: Vec2
                       , _transformScale :: Vec2
                       , _topIns :: [Tree TOP]
                       }

data PyType = PyFloat | Py

data Tree a where
  N :: (Op a) => a -> Tree a
  FC :: CHOP -> Tree CHOP -> (Tree CHOP -> Tree CHOP) -> (Tree CHOP -> Tree CHOP) -> Tree CHOP
  FT :: TOP -> Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP) -> Tree TOP
  Fix :: (Op a) => ByteString -> Tree a -> Tree a
  PyExpr :: ByteString -> Tree a
  ChopChan :: Int -> Tree CHOP -> Tree Float
  Mod :: (Num n) => (ByteString -> ByteString) -> Tree n -> Tree n
  Mod2 :: (Num n) => (ByteString -> ByteString -> ByteString) -> Tree n -> Tree n -> Tree n
  Cast :: (Num a, Num b) => (ByteString -> ByteString) -> Tree a -> Tree b
  Resolve :: Tree a -> Tree ByteString

type Vec2 = (Maybe (Tree Float), Maybe (Tree Float))
type Vec3 = (Maybe (Tree Float), Maybe (Tree Float), Maybe (Tree Float))
type IVec2 = (Maybe (Tree Int), Maybe (Tree Int))
emptyV3 = (Nothing, Nothing, Nothing)
emptyV2 = (Nothing, Nothing)


float :: Float -> Tree Float
float = PyExpr . pack . show

int :: Int -> Tree Int
int = PyExpr . pack . show

bool :: Bool -> Tree Bool
bool = PyExpr . Data.Bool.bool "0" "1"

add :: (Num a, Show a) => Tree a -> Tree a -> Tree a
add = Mod2 (\a b -> BS.concat ["(", a, "+", b, ")"])

makeLenses ''CHOP
makeLenses ''DAT
makeLenses ''TOP
makeLenses ''SOP

instance Op CHOP where
  pars n@(NoiseCHOP {..}) = catMaybes [ "roughness" <$$> _noiseCRoughness
                                      , "type" <$$> _noiseCType
                                      , "period" <$$> _noiseCPeriod
                                      ] ++ chopBasePars n
  pars n@(SOPToCHOP s) = [("sop", Resolve s)] ++ chopBasePars n
  pars n@(Logic p c _) = catMaybes ["preop" <$$> p, "convert" <$$> c] ++ chopBasePars n
  pars n@(ConstantCHOP v) = L.zipWith (\i v -> (BS.pack $ "value" ++ show i, Resolve v)) [0..] v ++ chopBasePars n
  pars n@(SelectCHOP c) = catMaybes [("chop" <$$> c)] ++ chopBasePars n
  pars n@(Count {..}) = catMaybes ["threshup" <$$> _countThresh, "output" <$$> _countLimType, "limitmin" <$$> _countLimMin, "limitmax" <$$> _countLimMax] ++ chopBasePars n
  pars n@(Fan o off _) = catMaybes ["fanop" <$$> o, "alloff" <$$> off] ++ chopBasePars n
  pars n@(Math a c _) = catMaybes ["preoff" <$$> a, "chopop" <$$> c] ++ chopBasePars n
  opType (SOPToCHOP _) = "sopToChop"
  opType (Logic {}) = "logic"
  opType (Hold {}) = "hold"
  opType (FeedbackCHOP _) = "feedbackChop"
  opType (SelectCHOP _) = "selectChop"
  opType (Count {}) = "count"
  opType (Fan {}) = "fan"
  opType (MergeCHOP _) = "mergeChop"
  opType (Math {}) = "math"
  opType (ConstantCHOP {}) = "constantChop"
  opType (NoiseCHOP {}) = "noiseChop"
  commands (Count {}) = [Pulse "reset"]
  commands _ = []
  connections (maybeToList . flip (^?) chopIns -> cs) = mconcat cs

instance Op DAT where
  pars (ChopExec chop offon won onoff woff vc ) = ("chop", Resolve chop):(catMaybes [ ("offtoon",) . PyExpr <$> offon
                                                                                    , ("whileon",) . PyExpr <$> won
                                                                                    , ("ontooff",) . PyExpr <$> onoff
                                                                                    , ("whileoff",) . PyExpr <$> woff
                                                                                    , ("valuechange",) . PyExpr <$> vc
                                                                                    ])
  pars (TextDAT _ f) = catMaybes [("file" <$$> f)]
  pars (TCPIPDAT m d f) = ("callbacks", Resolve d):(catMaybes [("mode" <$$> m), ("format" <$$> f)])
  opType (ChopExec _ _ _ _ _ _) = "chopExec"
  opType (TextDAT _ _) = "textDat"
  opType (Table _) = "table"
  opType (TCPIPDAT _ _ _) = "tcpip"
  text (Table t) = Just . BS.intercalate ("\n") . fmap (BS.intercalate ("\t")) . toLists $ t
  text (ChopExec _ offon won onoff woff vc) =
    Just . BS.intercalate "\n\n" . (traverse %~ concatFunc)
      $ catMaybes [ ("offToOn",) <$> offon
                  , ("whileOn",) <$> won
                  , ("onToOff",) <$> onoff
                  , ("whileOff",) <$>  woff
                  , ("valueChange",) <$> vc
                  ]
    where
      concatFunc (name, body) = BS.append (makec name) body
      makec prog = BS.concat ["def ", prog, "(channel, sampleIndex, val, prev): \n"]
  text (TextDAT t _) = t
  commands (TextDAT _ (isJust -> True)) = [Pulse "loadonstartpulse"]
  commands _ = []
  --connections (maybeToList . flip (^?) datIns -> cs) = mconcat cs

instance Op SOP where
  pars (CircleSOP p a) = catMaybes [ ("type" <$$> p) , ("arc" <$$> a)]
  pars (NoiseSOP t _) = vec3Map' "t" t
  pars (CHOPToSOP c a) = ("chop", Resolve c):(catMaybes [("attscope" <$$> a)])
  opType (CircleSOP _ _) = "circleSop"
  opType (NoiseSOP {}) = "noiseSop"
  opType (OutSOP {}) = "outSop"
  opType Sphere = "sphere"
  opType (CHOPToSOP _ _) = "chopToSop"
  connections (maybeToList . flip (^?) sopIns -> cs) = mconcat cs

instance Op TOP where
  pars (CHOPToTOP chop) = [("chop", Resolve chop)]
  pars (CompositeTOP op _) = [("operand", Resolve op)]
  pars (MovieFileIn file mode index) = ("file", file): (catMaybes [("playmode" <$$> mode) , ("index" <$$> index)])
  -- pars (Render geo cam light) = fromListMaybe [ ("geometry", Just $ Resolve geo)
  --                                               , ("camera", Just $ Resolve cam)
  --                                               , ("lights", ResolveT light)]
  pars (Transform t s _) = vec2Map' "t" t ++ vec2Map' "s" s
  pars (LevelTOP o _) = catMaybes [("opacity" <$$> o)]
  pars (NoiseTOP m r t) = (catMaybes [("mono" <$$> m)]) ++ (dimenMap "resolution" r) ++ vec3Map' "t" t
  pars (SwitchTOP i _) = [("index", Resolve i)]
  pars (Ramp t p r dat) = ("dat", Resolve  dat):(dimenMap "resolution" r) ++ (catMaybes [("type" <$$>  t), ("phase" <$$> p)])
  pars (SelectTOP c) = catMaybes [("top" <$$> c)]

  opType (CHOPToTOP _) = "chopToTop"
  opType (CompositeTOP {}) = "compositeTop"
  opType (Displace {}) = "displace"
  opType (MovieFileIn _ _ _) = "movieFileIn"
  -- opType (Render _ _ _) = "render"
  opType (Transform {}) = "transform"
  opType CircleTOP = "circleTop"
  opType (FeedbackTOP _) = "feedbackTop"
  opType (OutTOP {})= "outTop"
  opType (NullTOP {}) = "nullTop"
  opType (LevelTOP {}) = "levelTop"
  opType (NoiseTOP _ _ _) = "noiseTop"
  opType (Ramp _ _ _ _) = "ramp"
  opType (SwitchTOP {}) = "switchTop"
  opType (SelectTOP _) = "selectTop"
  connections (maybeToList . flip (^?) topIns -> cs) = mconcat cs
  commands _ = []

chopBasePars :: CHOP -> [(ByteString, (Tree ByteString))]
chopBasePars c = catMaybes [ "timeslice" <$$> (c ^? chopTimeSlice . _Just)]

(<$$>) :: ByteString -> Maybe (Tree a) -> Maybe (ByteString, Tree ByteString)
a <$$> b = (a,) . Resolve <$> b

vec2Map :: (ByteString, ByteString) -> String -> (Maybe (Tree a), Maybe (Tree a)) -> [(ByteString, Tree ByteString)]
vec2Map (x, y) n (xv, yv) = catMaybes [x <$$> xv,  y <$$> yv]

vec2Map' :: String -> Vec2 -> [(ByteString, Tree ByteString)]
vec2Map' = vec2Map ("x", "y")

dimenMap :: String -> IVec2 -> [(ByteString, Tree ByteString)]
dimenMap = vec2Map ("w", "h")

vec3Map :: (ByteString, ByteString, ByteString) -> String -> Vec3 -> [(ByteString, Tree ByteString)]
vec3Map (x, y, z) n (xv, yv, zv) = catMaybes [x <$$> xv,  y <$$> yv, z <$$> zv]

vec3Map' :: String -> Vec3 -> [(ByteString, Tree ByteString)]
vec3Map' = vec3Map ("x", "y", "z")

casti :: (Show f, Floating f, Integral i) => Tree f -> Tree i
casti = Cast (\fl -> BS.concat ["int(", fl, ")"])

-- CHOPs

noiseC :: (CHOP -> CHOP) -> Tree CHOP
noiseC f = N (f $ NoiseCHOP Nothing emptyV3 Nothing Nothing Nothing)

count' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
count' f t = N (f $ Count [t] Nothing Nothing Nothing Nothing Nothing)
count = count' id

selectC :: Tree CHOP -> Tree CHOP
selectC = N <$> SelectCHOP . Just

sopToC :: Tree SOP -> Tree CHOP
sopToC = N <$> SOPToCHOP

logic' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
logic' f = N <$> f . Logic Nothing Nothing
logic = logic' id

hold' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP -> Tree CHOP
hold' f h t = N <$> f $ Hold [h, t]
hold = hold' id

feedbackC :: Tree CHOP -> (Tree CHOP -> Tree CHOP) -> (Tree CHOP -> Tree CHOP) -> Tree CHOP
feedbackC = FC (FeedbackCHOP [])

fan' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
fan' f = N <$> f . Fan Nothing Nothing . (:[])
fan = fan' id

mergeC :: [Tree CHOP] -> Tree CHOP
mergeC = N <$> MergeCHOP

math' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
math' f = N <$> f . Math Nothing Nothing

opsadd :: CHOP -> CHOP
opsadd = mathCombChops .~ Just (int 1)

opaddf :: Float -> CHOP -> CHOP
opaddf a = mathAdd .~ Just (float a)

-- DATs

table :: Matrix BS.ByteString -> Tree DAT
table = N <$> Table

chopExec' :: (DAT -> DAT) -> Tree CHOP -> Tree DAT
chopExec' f chop = N $ f $ ChopExec chop Nothing Nothing Nothing Nothing Nothing

-- cell :: (Integral a, Integral b) => (Tree a, Tree b) -> Tree DAT -> Tree BS.ByteString
-- cell (x, y) t = Cell x y (treePar t)

-- cellf :: (Integral a, Integral b, Floating f, Show f) => (Tree a, Tree b) -> Tree DAT -> Tree f
-- cellf (x, y) t = Cell x y (treePar t)

textD :: String -> Tree DAT
textD t = N $ TextDAT (Just $ BS.pack t) Nothing

fileD :: String -> Tree DAT
fileD f = N (TextDAT Nothing (Just . PyExpr $ BS.pack ("\"" ++ f ++ "\"")))

tcpipD' :: (DAT -> DAT) -> Tree DAT -> Tree DAT
tcpipD' f d = N . f $ TCPIPDAT Nothing d Nothing

-- SOPs

circleS' :: (SOP -> SOP) -> Tree SOP
circleS' f = N . f $ CircleSOP Nothing Nothing

noiseS' :: (SOP -> SOP) -> Tree SOP -> Tree SOP
noiseS' f = N <$> f . NoiseSOP emptyV3 . (:[])

sphere' :: (SOP -> SOP) -> Tree SOP
sphere' f = N . f $ Sphere

outS :: Tree SOP -> Tree SOP
outS = N <$> OutSOP . (:[])

chopToS :: Tree CHOP -> Tree SOP
chopToS c = N $ CHOPToSOP c Nothing

-- Tops

movieFileIn' :: (TOP -> TOP) -> BS.ByteString -> Tree TOP
movieFileIn' f file = N . f $ (MovieFileIn (PyExpr file) Nothing Nothing)

displace :: Tree TOP -> Tree TOP -> Tree TOP
displace a b = N $ Displace [a, b]

chopToT :: Tree CHOP -> Tree TOP
chopToT = N <$> CHOPToTOP

outT :: Tree TOP -> Tree TOP
outT = N <$> OutTOP . (:[])

-- render :: Tree COMP -> Tree COMP -> Tree TOP
-- render geo cam = GeneratorTree (Render (TreePar geo) (TreePar cam) Nothing)

compT :: Int -> [Tree TOP] -> Tree TOP
compT op = N <$> CompositeTOP (int op)

feedbackT :: Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP) -> Tree TOP
feedbackT = FT (FeedbackTOP Nothing)

circleT' :: (TOP -> TOP) -> Tree TOP
circleT' f = N . f $ CircleTOP
circleT = circleT' id

transformT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
transformT' f = N <$> f . (Transform emptyV2 emptyV2) . (:[])
transformT = transformT' id

nullTop :: Tree TOP -> Tree TOP
nullTop = N . NullTOP . (:[])

levelT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
levelT' f = N <$> f. LevelTOP Nothing . (:[])

noiseT' :: (TOP -> TOP) -> Tree TOP
noiseT' f = N $ f $ NoiseTOP Nothing emptyV2 emptyV3

ramp :: [(Float, Float, Float, Float, Float)] -> Tree TOP
ramp = N <$> (Ramp Nothing Nothing emptyV2) . table . fromLists . fmap (^..each) . ((:) ("pos", "r", "g", "b", "a")) . fmap ((over each) (BS.pack . show))

switchTop :: Tree Float -> [Tree TOP] -> Tree TOP
switchTop i = N <$> SwitchTOP i

selectTop :: Tree TOP -> Tree TOP
selectTop = N <$> SelectTOP . Just


-- -- COMPs
-- instance Op COMP where
--   opType (Geo _ _ _ _) = "geo"
--   opType (Camera _) = "camera"
--   opType Light = "light"
--   opPars (Geo t s m us) = M.unions [fromListMaybe [("material", ResolveT m), ("scale", ResolveT us)], (vec3Map' "t" t), (vec3Map' "s" s)]
--   opPars (Camera t) = vec3Map' "t" t
--   opPars Light = M.empty
--   opText _ = Nothing
--   opCommands _ = []

-- geo :: Tree SOP -> Tree COMP
-- geo = ComponentTree (Geo emptyV3 emptyV3 Nothing Nothing)

-- cam :: Tree COMP
-- cam = GeneratorTree (Camera emptyV3)

-- light :: Tree COMP
-- light = GeneratorTree Light

-- -- Misc

-- fix :: (Op a) => BS.ByteString -> Tree a -> Tree a
-- fix = FixedTree
