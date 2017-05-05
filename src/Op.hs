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

import Control.Lens
import Data.Bool
import Data.Matrix
import Data.Maybe

import Data.ByteString.Char8 as BS
import Data.List as L

data CommandType = Pulse ByteString ByteString Int
                 | Store ByteString (Tree ByteString)

class (Op a) => Baseable a where
  inOp :: Tree a
  outOp :: Tree a -> Tree a

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

data CHOP = Analyze { _analyzeFunc :: Tree Int
                    , _chopIns :: [Tree CHOP]
                    }
          | AudioIn
          | AudioSpectrum { _chopIns :: [Tree CHOP] }
          | ConstantCHOP { _values :: [Tree Float] }
          | Count { _chopIns :: [Tree CHOP]
                  , _countReset :: Maybe (Tree CHOP)
                  , _countThresh :: Maybe (Tree Float)
                  , _countLimType :: Maybe (Tree Int)
                  , _countLimMin :: Maybe (Tree Float)
                  , _countLimMax :: Maybe (Tree Float)
                  , _countResetCondition :: Maybe (Tree Int)
                  }
          | Delay { _delayFrames :: Tree Int
                  , _chopIns :: [Tree CHOP]
                  }
          | Fan { _fanOp :: Maybe (Tree Int)
                , _fanOffNeg :: Maybe (Tree Bool)
                , _chopIns :: [Tree CHOP]
                }
          | FeedbackCHOP { _chopIns :: [Tree CHOP]
                         }
          | Hold { _chopIns :: [Tree CHOP]
                 }
          | InCHOP
          | Logic { _logicPreop :: Maybe (Tree Int)
                  , _logicConvert :: Maybe (Tree Int)
                  , _chopIns :: [Tree CHOP]
                  }
          | Math { _mathAdd :: Maybe (Tree Float)
                 , _mathAddPost :: Maybe (Tree Float)
                 , _mathAlign :: Maybe (Tree Int)
                 , _mathCombChops :: Maybe (Tree Int)
                 , _mathInt :: Maybe (Tree Int)
                 , _mathMult :: Maybe (Tree Float)
                 , _chopIns :: [Tree CHOP]
                 }
          | MergeCHOP { _mergeCDupes :: Maybe (Tree Int)
                      , _chopIns :: [Tree CHOP]
                      }
          | NoiseCHOP { _chopTimeSlice :: Maybe (Tree Bool)
                      , _noiseCTranslate :: Vec3
                      , _noiseCRoughness :: Maybe (Tree Float)
                      , _noiseCType :: Maybe (Tree Int)
                      , _noiseCPeriod :: Maybe (Tree Float)
                      }
          | OutCHOP { _chopIns :: [Tree CHOP]
                    }
          | SelectCHOP { _selectCChop :: Maybe (Tree CHOP)
                       }
          | SOPToCHOP { _sopToChopSop :: Tree SOP }
          | SwitchCHOP { _switchCIndex :: Tree Int
                       , _chopIns :: [Tree CHOP]
                       }
          | Timer { _timerSegments :: Maybe (Tree DAT)
                  , _timerShowSeg :: Maybe (Tree Bool)
                  , _timerShowRunning :: Maybe (Tree Bool)
                  }

data DAT = ChopExec { _chopExecChop :: Tree CHOP
                    , _ceOffToOn :: Maybe BS.ByteString
                    , _ceWhileOn :: Maybe BS.ByteString
                    , _ceOnToOff :: Maybe BS.ByteString
                    , _ceWhileOff :: Maybe BS.ByteString
                    , _ceValueChange :: Maybe BS.ByteString
                    }
         | DatExec { _datExecDat :: Tree DAT
                   , _deTableChange :: Maybe BS.ByteString
                   , _datVars :: [(ByteString, Tree ByteString)]
                   }
         | InDAT
         | OutDAT { _datIns :: [Tree DAT]
                  }
         | SelectDAT { _selectDRI :: Maybe (Tree Int)
                     , _selectDRStartI :: Maybe (Tree Int)
                     , _selectDREndI :: Maybe (Tree Int)
                     , _selectDRStartN :: Maybe (Tree ByteString)
                     , _selectDREndN :: Maybe (Tree ByteString)
                     , _selectDRExpr :: Maybe (Tree ByteString)
                     , _selectDat :: Tree DAT
                     }
         | Table { _tableText :: Matrix ByteString
                 }
         | TCPIPDAT { _tcpipMode :: Maybe (Tree Int)
                    , _tcpipCallbacks :: Tree DAT
                    , _tcpipCallbackFormat :: Maybe (Tree Int)
                    , _datVars :: [(ByteString, Tree ByteString)]
                    }
         | TextDAT { _textBlob :: Maybe BS.ByteString
                   , _textFile :: Maybe (Tree BS.ByteString)
                   , _datVars :: [(ByteString, Tree ByteString)]
                   }

data SOP = CHOPToSOP { _chopToSopChop :: Tree CHOP
                     , _chopToSopAttrScope :: Maybe (Tree BS.ByteString)
                     , _chopToSResample :: Maybe (Tree Bool)
                     , _sopIns :: [Tree SOP]
                     }
         | CircleSOP { _circType :: Maybe (Tree Int)
                     , _circArc :: Maybe (Tree Int)
                     , _sopIns :: [Tree SOP]
                     }
         | InSOP
         | NoiseSOP { _noiseSTranslate :: Vec3
                    , _sopIns :: [Tree SOP]
                    }
         | OutSOP { _sopIns :: [Tree SOP]
                  }
         | Sphere

data TOP = CHOPToTOP { _chopToTopChop :: Tree CHOP }
           | CircleTOP
           | CompositeTOP { _compTOperand :: Tree Int
                          , _topIns :: [Tree TOP]
                          }
           | Displace { _topIns :: [Tree TOP] }
           | FeedbackTOP { _fbTop :: Maybe (Tree TOP)
                         , _topIns :: [Tree TOP]
                         }
           | GLSLTOP { _glslTDat :: Tree DAT
                     , _glslTUniforms :: [(String, Vec4)]
                     , _topIns :: [Tree TOP]
                     }
           | LevelTOP { _levelOpacity :: Maybe (Tree Float)
                      , _topIns :: [Tree TOP]
                      }
           | InTOP
           | MovieFileIn { _movieFileInFile :: Tree BS.ByteString
                         , _moviePlayMode :: Maybe (Tree Int)
                         , _movieIndex :: Maybe (Tree Int)
                         }
           | NoiseTOP { _noiseTMonochrome :: Maybe (Tree Bool)
                      , _noiseTResolution :: IVec2
                      , _noiseTTranslate :: Vec3
                      }
           | NullTOP { _topIns :: [Tree TOP]}
           | OutTOP { _topIns :: [Tree TOP] }
           | Ramp { _rampType :: Maybe (Tree Int)
                  , _rampPhase :: Maybe (Tree Float)
                  , _rampResolution :: IVec2
                  , _rampValues :: Tree DAT
                  }
           | Render { _renderGeo :: Tree Geo
                   , _renderCamera :: Tree Camera
                   , _renderLight :: Maybe (Tree Light)
                   }
           | SelectTOP { _selectTTop :: Maybe (Tree TOP)
                       }
           | SwitchTOP { _switchTIndex :: Tree Float
                       , _topIns :: [Tree TOP]
                       }
           | Transform { _transformTranslate :: Vec2
                       , _transformScale :: Vec2
                       , _topIns :: [Tree TOP]
                       }

data MAT = ConstantMAT { _constColor :: Vec3
                       , _constAlpha :: Maybe (Tree Float)
                       }
         | InMAT
         | OutMAT { _matIns :: [Tree MAT]
                  }

data Geo = Geo { _geoTranslate :: Vec3
                , _geoScale :: Vec3
                , _geoMat :: Maybe (Tree MAT)
                , _geoUniformScale :: Maybe (Tree Float)
                }

data Camera = Camera { _camTranslate :: Vec3
                     }

data BaseCOMP = BaseCOMP

data Light = Light

data Tree a where
  N :: (Op a) => a -> Tree a
  FC :: CHOP -> Tree CHOP -> (Tree CHOP -> Tree CHOP) -> (Tree CHOP -> Tree CHOP) -> Tree CHOP
  FT :: TOP -> Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP) -> Tree TOP
  Comp :: (Op a, Op b) => a -> Tree b -> Tree a
  BComp :: (Baseable a, Baseable b) => BaseCOMP -> (Tree a -> Tree b) -> Tree a -> Tree b
  Fix :: (Op a) => ByteString -> Tree a -> Tree a
  PyExpr :: ByteString -> Tree a
  ChopChan :: ByteString -> Tree CHOP -> Tree Float
  Cell :: (Integral a, Integral b) => (Tree a, Tree b) -> Tree DAT -> Tree ByteString
  NumRows :: Tree DAT -> Tree Int
  Mod :: (Num n) => (ByteString -> ByteString) -> Tree n -> Tree n
  Mod2 :: (Num n) => (ByteString -> ByteString -> ByteString) -> Tree n -> Tree n -> Tree n
  Cast :: (Num b) => (ByteString -> ByteString) -> Tree a -> Tree b
  Resolve :: Tree a -> Tree ByteString
  ResolveP :: Tree a -> Tree ByteString

float :: Float -> Tree Float
float = PyExpr . pack . show

int :: Int -> Tree Int
int = PyExpr . pack . show

bool :: Bool -> Tree Bool
bool = PyExpr . Data.Bool.bool "0" "1"

str :: String -> Tree ByteString
str = PyExpr . pack . show

(!+) :: (Num a, Show a) => Tree a -> Tree a -> Tree a
(!+) = Mod2 (\a b -> BS.concat ["(", a, "+", b, ")"])

(!*) :: (Num a, Show a) => Tree a -> Tree a -> Tree a
(!*) = Mod2 (\a b -> BS.concat ["(", a, "*", b, ")"])

(!%) :: (Num a, Show a) => Tree a -> Tree a -> Tree a
(!%) = Mod2 (\a b -> BS.concat ["(", a, "%", b, ")"])

seconds :: Tree Float
seconds = PyExpr "absTime.seconds"

frames :: Tree Int
frames = PyExpr "absTime.frame"

floor :: (Num n) => Tree n -> Tree n
floor = pyMathOp "floor"

ceil :: (Num n) => Tree n -> Tree n
ceil = pyMathOp "ceil"

osin :: (Num n) => Tree n -> Tree n
osin = pyMathOp "sin"

pmax :: (Num n) => Tree n -> Tree n -> Tree n
pmax = Mod2 (\s t -> BS.concat ["max(", s, ", ", t, ")"])

pyMathOp :: (Num n) => String -> Tree n -> Tree n
pyMathOp s = Mod (\n -> BS.concat ["math.", pack s, "(", n, ")"])

chopChan0 :: Tree CHOP -> Tree Float
chopChan0 = ChopChan (pack . show $ 0)

chopChanName :: String -> Tree CHOP -> Tree Float
chopChanName s = ChopChan (pack $ "\"" ++ s ++ "\"")

numRows :: Tree DAT -> Tree Int
numRows = NumRows

type Vec2 = (Maybe (Tree Float), Maybe (Tree Float))
type Vec3 = (Maybe (Tree Float), Maybe (Tree Float), Maybe (Tree Float))
type Vec4 = (Maybe (Tree Float), Maybe (Tree Float), Maybe (Tree Float), Maybe (Tree Float))
type IVec2 = (Maybe (Tree Int), Maybe (Tree Int))

emptyV4 = (Nothing, Nothing, Nothing, Nothing)
emptyV3 = (Nothing, Nothing, Nothing)
emptyV2 = (Nothing, Nothing)

vec2Map :: (ByteString, ByteString) -> String -> (Maybe (Tree a), Maybe (Tree a)) -> [(ByteString, Tree ByteString)]
vec2Map (x, y) n (xv, yv) = catMaybes [BS.append (pack n) x <$$> xv,  BS.append (pack n) y <$$> yv]

vec2Map' :: String -> Vec2 -> [(ByteString, Tree ByteString)]
vec2Map' = vec2Map ("x", "y")

dimenMap :: String -> IVec2 -> [(ByteString, Tree ByteString)]
dimenMap = vec2Map ("w", "h")

rgbMap :: String -> Vec3 -> [(ByteString, Tree ByteString)]
rgbMap = vec3Map ("r", "g", "b")

vec4Map :: (ByteString, ByteString, ByteString, ByteString) -> String -> Vec4 -> [(ByteString, Tree ByteString)]
vec4Map (x, y, z, w) n (xv, yv, zv, wv) = catMaybes [ BS.append (pack n) x <$$> xv
                                                    , BS.append (pack n) y <$$> yv
                                                    , BS.append (pack n) z <$$> zv
                                                    , BS.append (pack n) w <$$> wv
                                                    ]
vec4Map' = vec4Map ("x", "y", "z", "w")

vec3Map :: (ByteString, ByteString, ByteString) -> String -> Vec3 -> [(ByteString, Tree ByteString)]
vec3Map (x, y, z) n (xv, yv, zv) = catMaybes [BS.append (pack n) x <$$> xv,  BS.append (pack n) y <$$> yv, BS.append (pack n) z <$$> zv]

vec3Map' :: String -> Vec3 -> [(ByteString, Tree ByteString)]
vec3Map' = vec3Map ("x", "y", "z")

fix :: (Op a) => BS.ByteString -> Tree a -> Tree a
fix = Fix

(<$$>) :: ByteString -> Maybe (Tree a) -> Maybe (ByteString, Tree ByteString)
a <$$> b = (a,) . Resolve <$> b

makeLenses ''CHOP
makeLenses ''DAT
makeLenses ''MAT
makeLenses ''SOP
makeLenses ''TOP

makeLenses ''Camera
makeLenses ''Geo
makeLenses ''Light

instance Op CHOP where
  pars n@(Analyze {..}) = [("function", Resolve _analyzeFunc)]
  pars n@(ConstantCHOP v) = L.zipWith (\i v' -> (BS.pack $ "value" ++ show i, Resolve v')) [0..] v ++ chopBasePars n
  pars n@(Count {..}) = catMaybes [ "threshup" <$$> _countThresh
                                  , "output" <$$> _countLimType
                                  , "limitmin" <$$> _countLimMin
                                  , "limitmax" <$$> _countLimMax
                                  , "resetcondition" <$$> _countResetCondition
                                  ] ++ chopBasePars n
  pars n@(Delay {..}) = [("delayunit", Resolve $ int 1), ("delay", Resolve _delayFrames)]
  pars n@(Fan o off _) = catMaybes ["fanop" <$$> o, "alloff" <$$> off] ++ chopBasePars n
  pars n@(Logic p c _) = catMaybes ["preop" <$$> p, "convert" <$$> c] ++ chopBasePars n
  pars n@(Math {..}) = catMaybes [ "preoff" <$$> _mathAdd
                                 , "postoff" <$$> _mathAddPost
                                 , "chopop" <$$> _mathCombChops
                                 , "integer" <$$> _mathInt
                                 , "align" <$$> _mathAlign
                                 , "gain" <$$> _mathMult
                                 ] ++ chopBasePars n
  pars n@(MergeCHOP {..}) = catMaybes [("duplicate" <$$> _mergeCDupes)] ++ chopBasePars n
  pars n@(NoiseCHOP {..}) = catMaybes [ "roughness" <$$> _noiseCRoughness
                                      , "type" <$$> _noiseCType
                                      , "period" <$$> _noiseCPeriod
                                      ] ++ chopBasePars n
  pars n@(SelectCHOP c) = catMaybes [("chop" <$$> c)] ++ chopBasePars n
  pars n@(SOPToCHOP s) = [("sop", ResolveP s)] ++ chopBasePars n
  pars n@(SwitchCHOP {..}) = [("index", Resolve _switchCIndex)] ++ chopBasePars n
  pars n@(Timer {..}) = catMaybes [("segdat",) . ResolveP <$> _timerSegments
                                  , ("outseg" <$$> _timerShowSeg)
                                  , ("outrunning" <$$> _timerShowSeg)
                                  ] ++ chopBasePars n
  pars _ = []
  opType (Analyze {}) = "analyze"
  opType (AudioIn {}) = "audioIn"
  opType (AudioSpectrum {}) = "audioSpectrum"
  opType (ConstantCHOP {}) = "constantChop"
  opType (Count {}) = "count"
  opType (Delay {}) = "delay"
  opType (Fan {}) = "fan"
  opType (FeedbackCHOP _) = "feedbackChop"
  opType (Hold {}) = "hold"
  opType (InCHOP {}) = "inChop"
  opType (Logic {}) = "logic"
  opType (MergeCHOP {}) = "mergeChop"
  opType (Math {}) = "math"
  opType (NoiseCHOP {}) = "noiseChop"
  opType (OutCHOP {}) = "outChop"
  opType (SwitchCHOP {}) = "switchChop"
  opType (SelectCHOP _) = "selectChop"
  opType (SOPToCHOP _) = "sopToChop"
  opType (Timer {}) = "timer"
  commands (Count {}) = [Pulse "resetpulse" "1" 1]
  commands _ = []
  connections (maybeToList . flip (^?) chopIns -> cs) = mconcat cs

instance Baseable CHOP where
  inOp = N $ InCHOP
  outOp o = N $ OutCHOP [o]

instance Op DAT where
  pars (ChopExec chop offon won onoff woff vc ) = ("chop", ResolveP chop):(catMaybes [ ("offtoon",) . Resolve . Op.bool . const True <$> offon
                                                                                    , ("whileon",) . Resolve . Op.bool . const True <$> won
                                                                                    , ("ontooff",) . Resolve . Op.bool . const True <$> onoff
                                                                                    , ("whileoff",) . Resolve . Op.bool . const True <$> woff
                                                                                    , ("valuechange",) . Resolve . Op.bool . const True <$> vc
                                                                                    ])
  pars (DatExec {..}) = ("dat", ResolveP _datExecDat):(catMaybes [("tablechange",) . Resolve . Op.bool . const True <$> _deTableChange])
  pars (TextDAT {..}) = catMaybes [("file" <$$> _textFile)]
  pars (TCPIPDAT m d f _) = ("callbacks", ResolveP d):(catMaybes [("mode" <$$> m), ("format" <$$> f)])
  pars (SelectDAT {..}) = maybe altChoice (\row -> [("rowindexstart", Resolve row), ("rowindexend", Resolve row), ("extractrows", Resolve $ int 2)]) _selectDRI ++ [("dat", ResolveP _selectDat)]
                          where
                            altChoice = catMaybes [ ("rownamestart" <$$> _selectDRStartN)
                                    , ("rowindexstart" <$$> _selectDRStartI)
                                    , ("rownameend" <$$> _selectDREndN)
                                    , ("rowindexend" <$$> _selectDREndI)
                                    , ("rowexpr" <$$> _selectDRExpr)
                                    ] ++ [("extractrows", Resolve . int $ chooseType _selectDRExpr _selectDRStartN _selectDRStartI _selectDREndN _selectDREndI)]
                            chooseType (Just _) _ _ _ _ = 6
                            chooseType _ (Just _) Nothing (Just _) Nothing = 1
                            chooseType _ Nothing (Just _) Nothing (Just _) = 2
                            chooseType _ (Just _) Nothing Nothing (Just _) = 3
                            chooseType _ Nothing (Just _) (Just _) Nothing = 4
                            chooseType _ _ _ _ _ = 0
  pars _ = []
  opType (ChopExec _ _ _ _ _ _) = "chopExec"
  opType (DatExec {}) = "datExec"
  opType (InDAT {}) = "inDat"
  opType (OutDAT {}) = "outDat"
  opType (SelectDAT {}) = "selectDat"
  opType (TextDAT {}) = "textDat"
  opType (Table _) = "table"
  opType (TCPIPDAT _ _ _ _) = "tcpip"
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
      makec prog = BS.concat ["def ", prog, "(channel, sampleIndex, val, prev):\n"]
  text (DatExec {..}) = Just . BS.intercalate "\n\n" $ catMaybes [ BS.append "def tableChange(dat):\n" <$> _deTableChange]
  text (TextDAT {..}) = _textBlob
  text _ = Nothing
  commands (TextDAT _ f cs) = (maybeToList $ const (Pulse "loadonstartpulse" "1" 1) <$> f) ++ (uncurry Store <$> cs)
  commands (DatExec {..}) = [Pulse "active" "0" 2] ++ (uncurry Store <$> _datVars)
  commands ((view datVars) -> tvs) = uncurry Store <$> tvs
  connections (maybeToList . flip (^?) datIns -> cs) = mconcat cs

instance Baseable DAT where
  inOp = N $ InDAT
  outOp o = N $ OutDAT [o]

instance Op MAT where
  pars (ConstantMAT rgb alpha) = catMaybes [("alpha" <$$> alpha)] ++ rgbMap "color" rgb
  opType (ConstantMAT _ _) = "constMat"
  connections (maybeToList . flip (^?) matIns -> cs) = mconcat cs

instance Baseable MAT where
  inOp = N $ InMAT
  outOp o = N $ OutMAT [o]

instance Op SOP where
  pars (CircleSOP p a _) = catMaybes [ ("type" <$$> p) , ("arc" <$$> a)]
  pars (NoiseSOP t _) = vec3Map' "t" t
  pars (CHOPToSOP c a r _) = ("chop", ResolveP c):(catMaybes [("attscope" <$$> a), ("mapping" <$$> r)])
  pars _ = []
  opType (CircleSOP {}) = "circleSop"
  opType (InSOP {}) = "inSop"
  opType (NoiseSOP {}) = "noiseSop"
  opType (OutSOP {}) = "outSop"
  opType Sphere = "sphere"
  opType (CHOPToSOP {}) = "chopToSop"
  connections (maybeToList . flip (^?) sopIns -> cs) = mconcat cs

instance Baseable SOP where
  inOp = N $ InSOP
  outOp o = N $ OutSOP [o]

instance Op TOP where
  pars (CHOPToTOP chop) = [("chop", ResolveP chop)]
  pars (CompositeTOP op _) = [("operand", Resolve op)]
  pars (FeedbackTOP {..}) = catMaybes [("top",) . ResolveP <$> _fbTop]
  pars (GLSLTOP {..}) = (:) ("pixeldat", ResolveP _glslTDat) $
    L.concatMap (\(i, (n, v)) -> (BS.pack $ "uniname" ++ show i, str n):vec4Map' ("value" ++ show i) v) $ L.zip [0..] _glslTUniforms
  pars (MovieFileIn file mode index) = ("file", file): (catMaybes [("playmode" <$$> mode) , ("index" <$$> index)])
  pars (Transform t s _) = vec2Map' "t" t ++ vec2Map' "s" s
  pars (LevelTOP o _) = catMaybes [("opacity" <$$> o)]
  pars (NoiseTOP m r t) = (catMaybes [("mono" <$$> m)]) ++ (dimenMap "resolution" r) ++ vec3Map' "t" t
  pars (SwitchTOP i _) = [("index", Resolve i)]
  pars (Ramp t p r dat) = ("dat", Resolve  dat):(dimenMap "resolution" r) ++ (catMaybes [("type" <$$>  t), ("phase" <$$> p)])
  pars (Render {..}) =  [("geometry", ResolveP _renderGeo), ("camera", ResolveP _renderCamera)] ++ maybeToList (("light",) . ResolveP <$> _renderLight)
  pars (SelectTOP c) = catMaybes [("top" <$$> c)]
  pars _ = []

  opType (CHOPToTOP _) = "chopToTop"
  opType (CompositeTOP {}) = "compositeTop"
  opType (Displace {}) = "displace"
  opType (GLSLTOP {}) = "glslTop"
  opType (InTOP {}) = "inTop"
  opType (MovieFileIn _ _ _) = "movieFileIn"
  -- opType (Render _ _ _) = "render"
  opType (Transform {}) = "transform"
  opType CircleTOP = "circleTop"
  opType (FeedbackTOP {}) = "feedbackTop"
  opType (OutTOP {})= "outTop"
  opType (NullTOP {}) = "nullTop"
  opType (LevelTOP {}) = "levelTop"
  opType (NoiseTOP _ _ _) = "noiseTop"
  opType (Ramp _ _ _ _) = "ramp"
  opType (Render {}) = "render"
  opType (SwitchTOP {}) = "switchTop"
  opType (SelectTOP _) = "selectTop"
  connections (maybeToList . flip (^?) topIns -> cs) = mconcat cs

instance Baseable TOP where
  inOp = N $ InTOP
  outOp o = N $ OutTOP [o]

instance Op Geo where
  opType (Geo _ _ _ _) = "geo"
  pars (Geo t s m us) = mconcat [catMaybes [("material",) . ResolveP <$> m, ("scale" <$$> us)], (vec3Map' "t" t), (vec3Map' "s" s)]

instance Op Camera where
  opType (Camera _) = "camera"
  pars (Camera t) = vec3Map' "t" t

instance Op Light where
  opType Light = "light"

instance Op BaseCOMP where
  opType BaseCOMP = "base"

-- Constructors

chopBasePars :: CHOP -> [(ByteString, (Tree ByteString))]
chopBasePars c = catMaybes [ "timeslice" <$$> (c ^? chopTimeSlice . _Just)]

casti :: (Integral i) => Tree f -> Tree i
casti = Cast (\fl -> BS.concat ["int(", fl, ")"])

castf :: (Floating f) => Tree i -> Tree f
castf = Cast (\fl -> BS.concat ["float(", fl, ")"])

-- CHOPs

analyze :: Tree Int -> Tree CHOP -> Tree CHOP
analyze f c = N $ Analyze f [c]

audioIn :: Tree CHOP
audioIn = N $ AudioIn

audioSpectrum :: Tree CHOP -> Tree CHOP
audioSpectrum t = N $ AudioSpectrum [t]

constC :: [Tree Float] -> Tree CHOP
constC = N <$> ConstantCHOP

count' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
count' f t = N ins
  where
    def = f $ Count [t] Nothing Nothing Nothing Nothing Nothing Nothing
    ins = def & chopIns %~ (flip (++) (catMaybes . maybeToList $ def ^? countReset))
count = count' id

delay :: Tree Int -> Tree CHOP -> Tree CHOP
delay f = N <$> Delay f . (:[])

feedbackC :: Tree CHOP -> (Tree CHOP -> Tree CHOP) -> (Tree CHOP -> Tree CHOP) -> Tree CHOP
feedbackC = FC (FeedbackCHOP [])

fan' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
fan' f = N <$> f . Fan Nothing Nothing . (:[])
fan = fan' id

hold' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP -> Tree CHOP
hold' f h t = N <$> f $ Hold [h, t]
hold = hold' id

logic' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
logic' f = N <$> f . Logic Nothing Nothing
logic = logic' id

mergeC' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
mergeC' f = N . f <$> MergeCHOP Nothing
mergeC = mergeC' id

math' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
math' f = N <$> f . Math Nothing Nothing Nothing Nothing Nothing Nothing

noiseC' :: (CHOP -> CHOP) -> Tree CHOP
noiseC' f = N (f $ NoiseCHOP Nothing emptyV3 Nothing Nothing Nothing)
noiseC = noiseC' id

opsadd :: CHOP -> CHOP
opsadd = mathCombChops .~ Just (int 1)

opaddf :: Float -> CHOP -> CHOP
opaddf a = mathAdd .~ Just (float a)

sopToC :: Tree SOP -> Tree CHOP
sopToC = N <$> SOPToCHOP

selectC :: Tree CHOP -> Tree CHOP
selectC = N <$> SelectCHOP . Just

switchC :: Tree Int -> [Tree CHOP] -> Tree CHOP
switchC i = N <$> SwitchCHOP i

data TimerSegment = TimerSegment { segDelay :: Float
                                 , segLength :: Float
                                 }

timerBS :: TimerSegment -> [ByteString]
timerBS (TimerSegment {..}) = [pack $ show segDelay, pack $ show segLength]

timerSeg' :: (CHOP -> CHOP) -> [TimerSegment] -> Tree CHOP
timerSeg' f ts = N . f $ Timer (Just $ table . fromLists $ ["delay", "length"]:(timerBS <$> ts)) Nothing Nothing
timerSeg = timerSeg' id

-- DATs

table :: Matrix BS.ByteString -> Tree DAT
table = N <$> Table

chopExec' :: (DAT -> DAT) -> Tree CHOP -> Tree DAT
chopExec' f chop = N $ f $ ChopExec chop Nothing Nothing Nothing Nothing Nothing

datExec' :: (DAT -> DAT) -> Tree DAT -> Tree DAT
datExec' f d = N $ f $ DatExec d Nothing []

cell :: (Integral a, Integral b) => (Tree a, Tree b) -> Tree DAT -> Tree BS.ByteString
cell = Cell

-- cellf :: (Integral a, Integral b, Floating f, Show f) => (Tree a, Tree b) -> Tree DAT -> Tree f
-- cellf (x, y) t = Cell x y (treePar t)

textD' :: (DAT -> DAT) -> String -> Tree DAT
textD' f t = N . f $ TextDAT (Just $ BS.pack t) Nothing []
textD = textD' id

fileD' :: (DAT -> DAT) -> String -> Tree DAT
fileD' f file = N . f $ (TextDAT Nothing (Just . PyExpr $ BS.pack ("\"" ++ file ++ "\"")) [])
fileD = fileD' id

selectD' :: (DAT -> DAT) -> Tree DAT -> Tree DAT
selectD' f t = N . f $ SelectDAT Nothing Nothing Nothing Nothing Nothing Nothing t

tcpipD' :: (DAT -> DAT) -> Tree DAT -> Tree DAT
tcpipD' f d = N . f $ TCPIPDAT Nothing d Nothing []

-- MATs

constM :: (MAT -> MAT) -> Tree MAT
constM f = N . f $ ConstantMAT emptyV3 Nothing

-- SOPs

circleS' :: (SOP -> SOP) -> Tree SOP
circleS' f = N . f $ CircleSOP Nothing Nothing []
circleS = circleS' id

noiseS' :: (SOP -> SOP) -> Tree SOP -> Tree SOP
noiseS' f = N <$> f . NoiseSOP emptyV3 . (:[])
noiseS = noiseS' id

sphere' :: (SOP -> SOP) -> Tree SOP
sphere' f = N . f $ Sphere
sphere = sphere' id

outS :: Tree SOP -> Tree SOP
outS = N <$> OutSOP . (:[])

chopToS' :: (SOP -> SOP) -> Tree CHOP -> Tree SOP
chopToS' f c = N . f $ CHOPToSOP c Nothing Nothing []
chopToS = chopToS' id

-- Tops

movieFileIn' :: (TOP -> TOP) -> Tree ByteString -> Tree TOP
movieFileIn' f file = N . f $ (MovieFileIn file Nothing Nothing)
movieFileIn = movieFileIn' id

displace :: Tree TOP -> Tree TOP -> Tree TOP
displace a b = N $ Displace [a, b]

chopToT :: Tree CHOP -> Tree TOP
chopToT = N <$> CHOPToTOP

outT :: Tree TOP -> Tree TOP
outT = N <$> OutTOP . (:[])

circleT' :: (TOP -> TOP) -> Tree TOP
circleT' f = N . f $ CircleTOP
circleT = circleT' id

compT :: Int -> [Tree TOP] -> Tree TOP
compT op = N <$> CompositeTOP (int op)

feedbackT :: Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP) -> Tree TOP
feedbackT = FT (FeedbackTOP Nothing [])

glslT' :: (TOP -> TOP) -> Tree DAT -> Tree TOP
glslT' f d = N . f $ GLSLTOP d [] []
glslT = glslT' id

render' :: (TOP -> TOP) -> Tree Geo -> Tree Camera -> Tree TOP
render' f geo cam = N . f $ Render geo cam Nothing
render = render' id

transformT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
transformT' f = N <$> f . (Transform emptyV2 emptyV2) . (:[])
transformT = transformT' id

nullT :: Tree TOP -> Tree TOP
nullT = N . NullTOP . (:[])

levelT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
levelT' f = N <$> f. LevelTOP Nothing . (:[])

noiseT' :: (TOP -> TOP) -> Tree TOP
noiseT' f = N $ f $ NoiseTOP Nothing emptyV2 emptyV3

ramp :: [(Float, Float, Float, Float, Float)] -> Tree TOP
ramp = N <$> (Ramp Nothing Nothing emptyV2) . table . fromLists . fmap (^..each) . ((:) ("pos", "r", "g", "b", "a")) . fmap ((over each) (BS.pack . show))

switchT :: Tree Float -> [Tree TOP] -> Tree TOP
switchT i = N <$> SwitchTOP i

selectT :: Tree TOP -> Tree TOP
selectT = N <$> SelectTOP . Just


-- COMPs

geo' :: (Geo -> Geo) -> Tree SOP -> Tree Geo
geo' f = Comp (f $ Geo emptyV3 emptyV3 Nothing Nothing)

cam' :: (Camera -> Camera) -> Tree Camera
cam' f = N . f $ Camera emptyV3
cam = cam' id

light :: Tree Light
light = N Light

base :: (Baseable a, Baseable b) => (Tree a -> Tree b) -> Tree a -> Tree b
base = BComp BaseCOMP
