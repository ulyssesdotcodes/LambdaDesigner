{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaDesigner.Op where

import Prelude hiding (sin)

import Control.Lens
import Data.Bool
import Data.Matrix
import Data.Maybe
import Data.Monoid

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
  customPars :: a -> [(ByteString, Tree ByteString)]
  customPars _ = []
  text :: a -> Maybe ByteString
  text _ = Nothing
  opType :: a -> ByteString
  opType _ = ""
  commands :: a -> [CommandType]
  commands _ = []

data CHOP = Analyze { _analyzeFunc :: Tree Int
                    , _chopIns :: [Tree CHOP]
                    }
          | AudioDeviceOut { _audioDevOutVolume :: Maybe (Tree Float)
                           , _chopIns :: [ Tree CHOP ]
                           }
          | AudioFileIn { _audioFileInFile :: Tree ByteString
                        , _audioFileInVolume :: Maybe (Tree Float)
                        , _audioFileInPlayMode :: Maybe (Tree Int)
                        , _audioFileInIndex :: Maybe (Tree Int)
                        }
          | AudioFilter { _audioFilterPass :: Tree Int
                        , _audioFilterCutoff :: Maybe (Tree Float)
                        , _chopIns :: [Tree CHOP]
                        }
          | AudioIn
          | AudioMovie { _audioMovieFileInTOP :: Tree TOP
                       }
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
          | Lag { _lagLag :: Vec2
                , _chopIns :: [Tree CHOP]
                }
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
          | MidiIn
          | NoiseCHOP { _chopTimeSlice :: Maybe (Tree Bool)
                      , _noiseCTranslate :: Vec3
                      , _noiseCRoughness :: Maybe (Tree Float)
                      , _noiseCAmplitude :: Maybe (Tree Float)
                      , _noiseCType :: Maybe (Tree Int)
                      , _noiseCPeriod :: Maybe (Tree Float)
                      , _noiseCChannels :: Maybe (Tree ByteString)
                      }
          | NullCHOP { _nullCCookType :: Maybe (Tree Int)
                     , _chopIns :: [Tree CHOP]
                     }
          | OscInCHOP { _oscInCPort :: Tree ByteString
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
                  , _timerCount :: Maybe (Tree Int)
                  , _timerLengthFrames :: Maybe (Tree Int)
                  , _timerLengthSeconds :: Maybe (Tree Float)
                  , _timerCallbacks :: Maybe (Tree DAT)
                  , _timerStart :: Bool
                  , _timerInit :: Bool
                  , _timerCue :: Bool
                  , _timerOnDone :: Maybe (Tree Int)
                  , _timerShowFraction :: Maybe (Tree Bool)
                  , _timerCycle :: Maybe (Tree Bool)
                  , _timerCycleLimit :: Maybe (Tree Bool)
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
         | ScriptDAT { _scriptDatDat :: Tree DAT
                     , _datIns :: [Tree DAT]
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
         | LineSOP
         | Metaball { _metaballRadius :: Vec3
                    , _metaballCenter :: Vec3
                    }
         | MergeSOP { _sopIns :: [Tree SOP]
                    }
         | NoiseSOP { _noiseSTranslate :: Vec3
                    , _sopIns :: [Tree SOP]
                    }
         | OutSOP { _sopIns :: [Tree SOP]
                  }
         | Sphere  { _sphereType :: Maybe (Tree Int)
                   , _sopIns :: [Tree SOP]
                   }
         | Sweep { _sopIns :: [Tree SOP]
                 }
         | TransformSOP { _transformSUniformScale :: Maybe (Tree Float)
                        , _sopIns :: [Tree SOP]
                        }

data TOP = Blur { _blurSize :: Tree Float
                , _topIns :: [Tree TOP]
                , _topPasses :: Maybe (Tree Int)
                }
           | CHOPToTOP { _chopToTopChop :: Tree CHOP }
           | CircleTOP
           | CompositeTOP { _compTOperand :: Tree Int
                          , _topIns :: [Tree TOP]
                          , _topPasses :: Maybe (Tree Int)
                          , _topResolution :: IVec2
                          }
           | Crop { _cropLeft :: Maybe (Tree Float)
                  , _cropRight :: Maybe (Tree Float)
                  , _cropTop :: Maybe (Tree Float)
                  , _cropBottom :: Maybe (Tree Float)
                  , _topIns :: [Tree TOP]
                  }
           | Displace { _topIns :: [Tree TOP] }
           | Edges { _topIns :: [Tree TOP]
                   , _topPasses :: Maybe (Tree Int)
                   }
           | Flip { _flipx :: Maybe (Tree Bool)
                  , _flipy :: Maybe (Tree Bool)
                  , _flipFlop :: Maybe (Tree Int)
                  , _topIns :: [Tree TOP]
                  , _topPasses :: Maybe (Tree Int)
                  }
           | FeedbackTOP { _fbTop :: Maybe (Tree TOP)
                         , _topIns :: [Tree TOP]
                         }
           | GLSLTOP { _glslTDat :: Tree DAT
                     , _glslTUniforms :: [(String, Vec4)]
                     , _pixelFormat :: Maybe (Tree Int)
                     , _topResolution :: IVec2
                     , _topIns :: [Tree TOP]
                     , _topPasses :: Maybe (Tree Int)
                     }
           | HSVAdjust { _hsvAdjSatMult :: Maybe (Tree Float)
                       , _hsvAdjValMult :: Maybe (Tree Float)
                       , _hsvAdjHueOffset :: Maybe (Tree Float)
                       , _topIns :: [Tree TOP]
                       }
           | LevelTOP { _levelOpacity :: Maybe (Tree Float)
                      , _levelBrightness :: Maybe (Tree Float)
                      , _topIns :: [Tree TOP]
                      }
           | InTOP
           | MovieFileIn { _movieFileInFile :: Tree BS.ByteString
                         , _moviePlayMode :: Maybe (Tree Int)
                         , _movieIndex :: Maybe (Tree Int)
                         , _topResolution :: IVec2
                         }
           | NoiseTOP { _noiseTMonochrome :: Maybe (Tree Bool)
                      , _noiseTResolution :: IVec2
                      , _noiseTTranslate :: Vec3
                      }
           | NullTOP { _topIns :: [Tree TOP] }
           | OutTOP { _topIns :: [Tree TOP] }
           | Ramp { _rampType :: Maybe (Tree Int)
                  , _rampPhase :: Maybe (Tree Float)
                  , _topResolution :: IVec2
                  , _rampValues :: Tree DAT
                  }
           | Render { _renderGeo :: Tree Geo
                   , _renderCamera :: Tree Camera
                   , _renderLight :: Maybe (Tree Light)
                   }
           | SelectTOP { _selectTTop :: Maybe (Tree TOP)
                       }
           | SwitchTOP { _switchTIndex :: Tree Float
                       , _switchTBlend :: Maybe (Tree Bool)
                       , _topIns :: [Tree TOP]
                       }
           | TextTOP { _textText :: Tree ByteString
                     , _topResolution :: IVec2
                     }
           | TransformTOP { _transformTranslate :: Vec2
                       , _transformExtend :: Maybe (Tree Int)
                       , _transformScale :: Vec2
                       , _transformRotate :: Maybe (Tree Float)
                       , _topPasses :: Maybe (Tree Int)
                       , _topIns :: [Tree TOP]
                       }
           | VideoDeviceIn

data MAT = ConstantMAT { _constColor :: Vec3
                       , _constAlpha :: Maybe (Tree Float)
                       , _constMatMap :: Maybe (Tree TOP)
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

data BaseCOMP = BaseCOMP { _baseParams :: [(ByteString, Tree ByteString)]
                         , _externalTox :: Maybe (Tree ByteString)
                         }

data Light = Light

data Tree a where
  N :: (Op a) => a -> Tree a
  FC :: CHOP -> Tree CHOP -> (Tree CHOP -> Tree CHOP) -> (Tree CHOP -> Tree CHOP) -> Tree CHOP
  FT :: TOP -> Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP) -> Tree TOP
  Comp :: (Op a, Op b) => a -> Tree b -> Tree a
  BComp :: (Baseable a, Baseable b) => BaseCOMP -> (Tree a -> Tree b) -> Tree a -> Tree b
  Tox :: (Op a, Op b) => BaseCOMP -> Maybe (Tree a) -> Tree b
  Fix :: (Op a) => ByteString -> Tree a -> Tree a
  PyExpr :: ByteString -> Tree a
  ChopChan :: ByteString -> Tree CHOP -> Tree Float
  Cell :: (Integral a, Integral b) => (Tree a, Tree b) -> Tree DAT -> Tree ByteString
  NumRows :: Tree DAT -> Tree Int
  Mod :: (ByteString -> ByteString) -> Tree n -> Tree n
  Mod2 :: (ByteString -> ByteString -> ByteString) -> Tree a -> Tree b -> Tree c
  Mod3 :: (ByteString -> ByteString -> ByteString -> ByteString) -> Tree a -> Tree b -> Tree c -> Tree d
  Cast :: (ByteString -> ByteString) -> Tree a -> Tree b
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

bstr :: String -> Tree ByteString
bstr = PyExpr . pack

(!+) :: (Show a) => Tree a -> Tree a -> Tree a
(!+) = Mod2 (\a b -> BS.concat ["(", a, "+", b, ")"])

(!*) :: (Show a) => Tree a -> Tree a -> Tree a
(!*) = Mod2 (\a b -> BS.concat ["(", a, "*", b, ")"])

(!%) :: (Show a) => Tree a -> Tree a -> Tree a
(!%) = Mod2 (\a b -> BS.concat ["(", a, "%", b, ")"])

(!==) :: Tree a -> Tree a -> Tree Bool
(!==) = Mod2 (\a b -> BS.concat ["(", a, "==", b, ")"])

ternary :: Tree Bool -> Tree a -> Tree a -> Tree a
ternary = Mod3 (\a b c -> BS.concat [b, " if ", a, " else ", c])

seconds :: Tree Float
seconds = PyExpr "absTime.seconds"

frames :: Tree Int
frames = PyExpr "absTime.frame"

scycle :: Float -> Float -> Tree Float
scycle a b =float b !* ((float a !* seconds) !% float 1)

sincycle :: Float -> Float -> Tree Float
sincycle a b =float b !* ((osin' $ float a !* seconds) !% float 1)

floor :: (Num n) => Tree n -> Tree n
floor = pyMathOp "floor"

ceil :: (Num n) => Tree n -> Tree n
ceil = pyMathOp "ceil"

osin :: (Num n) => Tree n -> Tree n
osin = pyMathOp "sin"
osin' = (!* float 0.5) . (!+ float 1) . osin

pmax :: (Num n) => Tree n -> Tree n -> Tree n
pmax = Mod2 (\s t -> BS.concat ["max(", s, ", ", t, ")"])

pyMathOp :: (Num n) => String -> Tree n -> Tree n
pyMathOp s = Mod (\n -> BS.concat ["math.", pack s, "(", n, ")"])

chopChan :: Int -> Tree CHOP -> Tree Float
chopChan n = ChopChan (pack . show $ n)

chopChan0 :: Tree CHOP -> Tree Float
chopChan0 = chopChan 0

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

xV4 :: Tree Float -> Vec4
xV4 x = emptyV4 & _1 ?~ x

xV3 :: Tree Float -> Vec3
xV3 x = emptyV3 & _1 ?~ x

iv2 :: (Int, Int) -> IVec2
iv2 (x, y) = (Just $ int x, Just $ int y)

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
  pars n@(AudioFilter {..}) = [("filter", Resolve _audioFilterPass)] ++
    catMaybes ["cutofflog" <$$> _audioFilterCutoff]
  pars n@(AudioDeviceOut {..}) = catMaybes ["volume" <$$> _audioDevOutVolume] ++ chopBasePars n
  pars n@(AudioFileIn {..}) = [("file", Resolve _audioFileInFile)] ++
                              catMaybes [ "volume" <$$> _audioFileInVolume
                                        , "index" <$$> _audioFileInIndex
                                        , "playmode" <$$> getFirst (First ( const (int 1) <$> _audioFileInIndex) <> First _audioFileInPlayMode)
                                        ]
  pars (AudioIn) = []
  pars (AudioMovie {..}) = [ ("moviefileintop", ResolveP _audioMovieFileInTOP)
                           ]
  pars (AudioSpectrum _) = []
  pars n@(ConstantCHOP v) = L.concat (L.zipWith (\i v' -> [(BS.pack $ "value" ++ show i, Resolve v'), (BS.pack $ "name" ++ show i, str $ "chan" ++ show i)]) [0..] v) ++ chopBasePars n
  pars n@(Count {..}) = catMaybes [ "threshup" <$$> _countThresh
                                  , "output" <$$> _countLimType
                                  , "limitmin" <$$> _countLimMin
                                  , "limitmax" <$$> _countLimMax
                                  , "resetcondition" <$$> _countResetCondition
                                  ] ++ chopBasePars n
  pars n@(Delay {..}) = [("delayunit", Resolve $ int 1), ("delay", Resolve _delayFrames)]
  pars n@(Fan o off _) = catMaybes ["fanop" <$$> o, "alloff" <$$> off] ++ chopBasePars n
  pars (FeedbackCHOP _) = []
  pars (Hold _) = []
  pars InCHOP = []
  pars n@(Lag {..}) = vec2Map ("1", "2") "lag" _lagLag ++ chopBasePars n
  pars n@(Logic p c _) = catMaybes ["preop" <$$> p, "convert" <$$> c] ++ chopBasePars n
  pars n@(Math {..}) = catMaybes [ "preoff" <$$> _mathAdd
                                 , "postoff" <$$> _mathAddPost
                                 , "chopop" <$$> _mathCombChops
                                 , "integer" <$$> _mathInt
                                 , "align" <$$> _mathAlign
                                 , "gain" <$$> _mathMult
                                 ] ++ chopBasePars n
  pars n@(MergeCHOP {..}) = catMaybes [("duplicate" <$$> _mergeCDupes)] ++ chopBasePars n
  pars MidiIn = []
  pars n@(NoiseCHOP {..}) = catMaybes [ "roughness" <$$> _noiseCRoughness
                                      , "type" <$$> _noiseCType
                                      , "period" <$$> _noiseCPeriod
                                      , "amp" <$$> _noiseCAmplitude
                                      , ("channelname",) <$> _noiseCChannels
                                      ] ++ chopBasePars n ++ vec3Map' "t" _noiseCTranslate
  pars n@(NullCHOP {..}) = catMaybes [("cooktype" <$$> _nullCCookType)] ++ chopBasePars n
  pars n@(OscInCHOP {..}) = [("port", _oscInCPort)]
  pars (OutCHOP _) = []
  pars n@(SelectCHOP c) = catMaybes [(("chop",) . ResolveP <$> c)] ++ chopBasePars n
  pars n@(SOPToCHOP s) = [("sop", ResolveP s)] ++ chopBasePars n
  pars n@(SwitchCHOP {..}) = [("index", Resolve _switchCIndex)] ++ chopBasePars n
  pars n@(Timer {..}) = catMaybes [ ("segdat",) . ResolveP <$> _timerSegments
                                  , ("callbacks",) . ResolveP <$> _timerCallbacks
                                  , ("length" <$$> _timerLengthSeconds)
                                  , ("length" <$$> _timerLengthFrames)
                                  , ("lengthunits",) . Resolve . const (int 2) <$> _timerLengthSeconds
                                  , ("lengthunits",) . Resolve . const (int 1) <$> _timerLengthFrames
                                  , ("outseg" <$$> _timerShowSeg)
                                  , ("outsegpulse" <$$> _timerShowSeg)
                                  , ("outrunning" <$$> _timerShowRunning)
                                  , ("outtimercount" <$$> _timerCount)
                                  , ("ondone" <$$> _timerOnDone)
                                  , ("outfraction" <$$> _timerShowFraction)
                                  , ("cycle" <$$> _timerCycle)
                                  , ("cyclelimit" <$$> _timerCycleLimit)
                                  ] ++ chopBasePars n
  opType (Analyze {}) = "analyze"
  opType (AudioDeviceOut {}) = "audioDevOut"
  opType (AudioFileIn {}) = "audioFileIn"
  opType (AudioMovie {}) = "audioMovie"
  opType (AudioFilter {}) = "audioFilter"
  opType (AudioIn {}) = "audioIn"
  opType (AudioSpectrum {}) = "audioSpectrum"
  opType (ConstantCHOP {}) = "constantChop"
  opType (Count {}) = "count"
  opType (Delay {}) = "delay"
  opType (Fan {}) = "fan"
  opType (FeedbackCHOP _) = "feedbackChop"
  opType (Hold {}) = "hold"
  opType (InCHOP {}) = "inChop"
  opType (Lag {}) = "lag"
  opType (Logic {}) = "logic"
  opType (Math {}) = "math"
  opType (MergeCHOP {}) = "mergeChop"
  opType (MidiIn {}) = "midiIn"
  opType (NoiseCHOP {}) = "noiseChop"
  opType (NullCHOP {}) = "nullChop"
  opType (OscInCHOP {}) = "oscInChop"
  opType (OutCHOP {}) = "outChop"
  opType (SwitchCHOP {}) = "switchChop"
  opType (SelectCHOP _) = "selectChop"
  opType (SOPToCHOP _) = "sopToChop"
  opType (Timer {}) = "timer"
  commands (Count {}) = [Pulse "resetpulse" "1" 1]
  commands (Timer {..}) = L.map fst . L.filter snd $ L.zip [Pulse "start" "1" 2, Pulse "cuepulse" "1" 1, Pulse "initialize" "1" 1] [_timerStart, _timerCue, _timerInit]
  commands _ = []
  connections (maybeToList . flip (^?) chopIns -> cs) = mconcat cs

chopBasePars :: CHOP -> [(ByteString, (Tree ByteString))]
chopBasePars c = catMaybes [ "timeslice" <$$> (c ^? chopTimeSlice . _Just)]

instance Baseable CHOP where
  inOp = N $ InCHOP
  outOp o = N $ OutCHOP [o]

instance Op DAT where
  pars (ChopExec chop offon won onoff woff vc ) = ("chop", ResolveP chop):(catMaybes [ ("offtoon",) . Resolve . LambdaDesigner.Op.bool . const True <$> offon
                                                                                    , ("whileon",) . Resolve . LambdaDesigner.Op.bool . const True <$> won
                                                                                    , ("ontooff",) . Resolve . LambdaDesigner.Op.bool . const True <$> onoff
                                                                                    , ("whileoff",) . Resolve . LambdaDesigner.Op.bool . const True <$> woff
                                                                                    , ("valuechange",) . Resolve . LambdaDesigner.Op.bool . const True <$> vc
                                                                                    ])
  pars (DatExec {..}) = ("dat", ResolveP _datExecDat):(catMaybes [("tablechange",) . Resolve . LambdaDesigner.Op.bool . const True <$> _deTableChange])
  pars (ScriptDAT {..}) = [("callbacks", ResolveP _scriptDatDat)]
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
  pars (TextDAT {..}) = catMaybes [("file" <$$> _textFile)]
  pars (TCPIPDAT m d f _) = ("callbacks", ResolveP d):(catMaybes [("mode" <$$> m), ("format" <$$> f)])
  pars _ = []
  opType (ChopExec _ _ _ _ _ _) = "chopExec"
  opType (DatExec {}) = "datExec"
  opType (InDAT {}) = "inDat"
  opType (OutDAT {}) = "outDat"
  opType (ScriptDAT {}) = "scriptDat"
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
  pars (ConstantMAT rgb alpha cmap) = catMaybes [("alpha" <$$> alpha), ("colormap",) . ResolveP <$> cmap]
    ++ rgbMap "color" rgb
  pars InMAT = []
  pars (OutMAT _) = []
  opType (ConstantMAT {}) = "constMat"
  opType InMAT = "inMat"
  opType (OutMAT _) = "outMat"
  connections (maybeToList . flip (^?) matIns -> cs) = mconcat cs

instance Baseable MAT where
  inOp = N $ InMAT
  outOp o = N $ OutMAT [o]

instance Op SOP where
  pars (CircleSOP p a _) = catMaybes [ ("type" <$$> p) , ("arc" <$$> a)]
  pars (CHOPToSOP c a r _) = ("chop", ResolveP c):(catMaybes [("attscope" <$$> a), ("mapping" <$$> r)])
  pars (Sphere p _) = catMaybes [ ("type" <$$> p) ]
  pars (Metaball {..}) = vec3Map' "rad" _metaballRadius ++ vec3Map' "t" _metaballCenter
  pars (NoiseSOP t _) = vec3Map' "t" t
  pars (TransformSOP {..}) = catMaybes ["scale" <$$> _transformSUniformScale]
  pars _ = []
  opType (CHOPToSOP {}) = "chopToSop"
  opType (CircleSOP {}) = "circleSop"
  opType (InSOP {}) = "inSop"
  opType (LineSOP {}) = "lineSop"
  opType (MergeSOP {}) = "mergeSop"
  opType (Metaball {}) = "metaball"
  opType (NoiseSOP {}) = "noiseSop"
  opType (OutSOP {}) = "outSop"
  opType (Sphere {}) = "sphere"
  opType (Sweep {}) = "sweep"
  opType (TransformSOP {}) = "transformSop"
  connections (maybeToList . flip (^?) sopIns -> cs) = mconcat cs

instance Baseable SOP where
  inOp = N $ InSOP
  outOp o = N $ OutSOP [o]

instance Op TOP where
  pars t@(Blur {..}) = [("size",) . Resolve $ _blurSize] ++ topBasePars t
  pars (CHOPToTOP chop) = [("chop", ResolveP chop)]
  pars t@(CompositeTOP {..}) = [("operand", Resolve _compTOperand)] ++ topBasePars t
  pars (Crop {..}) = catMaybes [ "cropleft" <$$> _cropLeft
                               , "cropright" <$$> _cropRight
                               , "croptop" <$$> _cropTop
                               , "cropbottom" <$$> _cropBottom
                               ]
  pars (Flip {..}) = catMaybes ["flipx" <$$> _flipx, "flipy" <$$> _flipy, "flop" <$$> _flipFlop]
  pars (FeedbackTOP {..}) = catMaybes [("top",) . ResolveP <$> _fbTop]
  pars t@(GLSLTOP {..}) = (++) (("pixeldat", ResolveP _glslTDat):(topBasePars t)) $
    L.concatMap (\(i, (n, v)) -> (BS.pack $ "uniname" ++ show i, str n):vec4Map' ("value" ++ show i) v) $ L.zip [0..] _glslTUniforms
  pars t@(MovieFileIn file mode index _) = ("file", file):
    (catMaybes ["playmode" <$$> getFirst (First ( const (int 1) <$> index) <> First mode), ("index" <$$> index)]) ++ topBasePars t
  pars (HSVAdjust {..}) = catMaybes [ "saturationmult" <$$> _hsvAdjSatMult
                                    , "valuemult" <$$> _hsvAdjValMult
                                    , "hueoffset" <$$> _hsvAdjHueOffset
                                    ]
  pars (LevelTOP {..}) = catMaybes [("opacity" <$$> _levelOpacity), "brightness1" <$$> _levelBrightness]
  pars (NoiseTOP m r t) = (catMaybes [("mono" <$$> m)]) ++ (dimenMap "resolution" r) ++ vec3Map' "t" t
  pars (SwitchTOP {..}) = [("index", Resolve _switchTIndex)] ++ catMaybes ["blend" <$$> _switchTBlend]
  pars (Ramp t p r dat) = ("dat", ResolveP dat):(dimenMap "resolution" r) ++ (catMaybes [("type" <$$>  t), ("phase" <$$> p)])
  pars (Render {..}) =  [("geometry", ResolveP _renderGeo), ("camera", ResolveP _renderCamera)] ++ maybeToList (("light",) . ResolveP <$> _renderLight)
  pars (SelectTOP c) = catMaybes [("top",) . ResolveP <$> c]
  pars t@(TextTOP {..}) = [("text", _textText)] ++ topBasePars t
  pars t@(TransformTOP {..}) = vec2Map' "t" _transformTranslate ++ vec2Map' "s" _transformScale ++
    catMaybes [ "rotate" <$$> _transformRotate
              , "extend" <$$> _transformExtend
              ] ++ topBasePars t
  pars _ = []

  opType (Blur {}) = "blur"
  opType (CHOPToTOP _) = "chopToTop"
  opType CircleTOP = "circleTop"
  opType (CompositeTOP {}) = "compositeTop"
  opType (Crop {}) = "crop"
  opType (Displace {}) = "displace"
  opType (Edges {}) = "edges"
  opType (FeedbackTOP {}) = "feedbackTop"
  opType (Flip {}) = "flip"
  opType (GLSLTOP {}) = "glslTop"
  opType (InTOP {}) = "inTop"
  opType (HSVAdjust {}) = "hsvAdjustTop"
  opType (LevelTOP {}) = "levelTop"
  opType (MovieFileIn {}) = "movieFileIn"
  opType (NoiseTOP _ _ _) = "noiseTop"
  opType (NullTOP {}) = "nullTop"
  opType (OutTOP {})= "outTop"
  opType (Ramp _ _ _ _) = "ramp"
  opType (Render {}) = "render"
  opType (SelectTOP _) = "selectTop"
  opType (SwitchTOP {}) = "switchTop"
  opType (TextTOP {}) = "textTop"
  opType (TransformTOP {}) = "transform"
  opType (VideoDeviceIn) = "videoDeviceIn"
  connections (maybeToList . flip (^?) topIns -> cs) = mconcat cs

topBasePars :: TOP -> [(ByteString, (Tree ByteString))]
topBasePars c = catMaybes [ "resolutionw" <$$> (c ^? topResolution._1._Just)
                          , "resolutionh" <$$> (c ^? topResolution._2._Just)
                          , "format" <$$> (c ^? pixelFormat._Just)
                          , "npasses" <$$> (c ^? topPasses._Just)
                          , ("outputresolution",) <$> (fmap (const (Resolve $ int 9)) $ safeHead $
                              catMaybes [c ^? topResolution._1._Just, c ^? topResolution._2._Just])
                          ]
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

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
  pars (BaseCOMP {..}) = catMaybes ["externaltox" <$$> _externalTox]
  customPars (BaseCOMP {..}) = _baseParams
  opType (BaseCOMP {}) = "base"
  commands (BaseCOMP {..}) = []

-- Constructors

casti :: (Integral i) => Tree f -> Tree i
casti = Cast (\fl -> BS.concat ["int(", fl, ")"])

castf :: (Floating f) => Tree i -> Tree f
castf = Cast (\fl -> BS.concat ["float(", fl, ")"])

caststr :: (Show a) => Tree a -> Tree ByteString
caststr = Cast (\s -> BS.concat ["str(", s, ")"])

-- CHOPs

analyze :: Tree Int -> Tree CHOP -> Tree CHOP
analyze f c = N $ Analyze f [c]

audioDevOut' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
audioDevOut' f = N <$> f . AudioDeviceOut Nothing . (:[])
audioDevOut = audioDevOut' id

audioFileIn' :: (CHOP -> CHOP) -> Tree ByteString -> Tree CHOP
audioFileIn' f file = N . f $ (AudioFileIn file Nothing Nothing Nothing)
audioFileIn = audioFileIn' id

audioMovie :: Tree TOP -> Tree CHOP
audioMovie movieTop = N $ AudioMovie movieTop

audioIn :: Tree CHOP
audioIn = N $ AudioIn

lowPass :: Tree CHOP -> Tree CHOP
lowPass t = N $ AudioFilter (int 0) Nothing [t]

highPass :: Tree CHOP -> Tree CHOP
highPass t = N $ AudioFilter (int 1) Nothing [t]

bandPass :: Tree Float -> Tree CHOP -> Tree CHOP
bandPass b t = N $ AudioFilter (int 2) (Just (b !* float 4.5)) [t]

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

lag' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
lag' f = N . f <$> Lag emptyV2 . (:[])
lag :: Tree Float -> Tree Float -> Tree CHOP -> Tree CHOP
lag a b t = N $ Lag (Just a, Just b) [t]

logic' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
logic' f = N <$> f . Logic Nothing Nothing
logic = logic' id

math' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
math' f = N <$> f . Math Nothing Nothing Nothing Nothing Nothing Nothing

mergeC' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
mergeC' f = N . f <$> MergeCHOP Nothing
mergeC = mergeC' id

mchan :: String -> Tree Float
mchan s = chopChanName s $ N MidiIn

noiseC' :: (CHOP -> CHOP) -> Tree CHOP
noiseC' f = N (f $ NoiseCHOP Nothing emptyV3 Nothing Nothing Nothing Nothing Nothing)
noiseC = noiseC' id

nullC' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
nullC' f = N <$> f . NullCHOP Nothing . (:[])
nullC = nullC' id
cookC = nullC' (nullCCookType ?~ int 1)

oscin :: Int -> Tree CHOP
oscin p = N $ OscInCHOP (Resolve $ int p)

opsadd :: CHOP -> CHOP
opsadd = mathCombChops .~ Just (int 1)

opaddf :: Float -> CHOP -> CHOP
opaddf a = mathAdd .~ Just (float a)

opmultf :: Float -> CHOP -> CHOP
opmultf a = mathMult .~ Just (float a)

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
timerSeg' f ts = N . f $ Timer (Just $ table . fromLists $ ["delay", "length"]:(timerBS <$> ts)) Nothing Nothing Nothing Nothing Nothing Nothing False False False Nothing Nothing Nothing Nothing
timerSeg = timerSeg' id

timerF' :: (CHOP -> CHOP) -> Tree Int -> Tree CHOP
timerF' f l = N . f $ Timer Nothing Nothing Nothing (Just $ int 1) (Just l) Nothing Nothing False False False Nothing Nothing Nothing Nothing

timerS' :: (CHOP -> CHOP) -> Tree Float -> Tree CHOP
timerS' f l = N . f $ Timer Nothing Nothing Nothing (Just $ int 2) Nothing (Just l) Nothing False False False Nothing Nothing Nothing Nothing

-- DATs

cell :: (Integral a, Integral b) => (Tree a, Tree b) -> Tree DAT -> Tree BS.ByteString
cell = Cell

chopExec' :: (DAT -> DAT) -> Tree CHOP -> Tree DAT
chopExec' f chop = N $ f $ ChopExec chop Nothing Nothing Nothing Nothing Nothing

datExec' :: (DAT -> DAT) -> Tree DAT -> Tree DAT
datExec' f d = N $ f $ DatExec d Nothing []

fileD' :: (DAT -> DAT) -> String -> Tree DAT
fileD' f file = N . f $ (TextDAT Nothing (Just . PyExpr $ BS.pack ("\"" ++ file ++ "\"")) [])
fileD = fileD' id

scriptD :: String -> Tree DAT -> Tree DAT
scriptD file = N <$> ScriptDAT (fileD file) . (:[])

selectD' :: (DAT -> DAT) -> Tree DAT -> Tree DAT
selectD' f t = N . f $ SelectDAT Nothing Nothing Nothing Nothing Nothing Nothing t

table :: Matrix BS.ByteString -> Tree DAT
table = N <$> Table

tcpipD' :: (DAT -> DAT) -> Tree DAT -> Tree DAT
tcpipD' f d = N . f $ TCPIPDAT Nothing d Nothing []

textD' :: (DAT -> DAT) -> String -> Tree DAT
textD' f t = N . f $ TextDAT (Just $ BS.pack t) Nothing []
textD = textD' id

-- MATs

constM' :: (MAT -> MAT) -> Tree MAT
constM' f = N . f $ ConstantMAT emptyV3 Nothing Nothing

topM :: Tree TOP -> Tree MAT
topM t = constM' (constMatMap ?~ t)

-- SOPs

circleS' :: (SOP -> SOP) -> Tree SOP
circleS' f = N . f $ CircleSOP Nothing Nothing []
circleS = circleS' id

lineS :: Tree SOP
lineS = N $ LineSOP

mergeS :: [Tree SOP] -> Tree SOP
mergeS = N . MergeSOP

metaball' :: (SOP -> SOP) -> Tree SOP
metaball' f = N . f $ Metaball emptyV3 emptyV3

noiseS' :: (SOP -> SOP) -> Tree SOP -> Tree SOP
noiseS' f = N <$> f . NoiseSOP emptyV3 . (:[])
noiseS = noiseS' id

sphere' :: (SOP -> SOP) -> Tree SOP
sphere' f = N . f $ Sphere Nothing []
sphere = sphere' id

sweep :: Tree SOP -> Tree SOP -> Tree SOP
sweep cross back = N $ Sweep [cross, back]

outS :: Tree SOP -> Tree SOP
outS = N <$> OutSOP . (:[])

chopToS' :: (SOP -> SOP) -> Tree CHOP -> Maybe (Tree SOP) -> Tree SOP
chopToS' f c i = N . f $ CHOPToSOP c Nothing Nothing (maybeToList i)
chopToS = chopToS' id

transformS' :: (SOP -> SOP) -> Tree SOP -> Tree SOP
transformS' f = N <$> f . (TransformSOP Nothing) . (:[])
transformS = transformS' id

scaleS :: Tree Float -> Tree SOP -> Tree SOP
scaleS f s = transformS' (transformSUniformScale ?~ f) s

-- TOPs

blur' :: (TOP -> TOP) -> Tree Float -> Tree TOP -> Tree TOP
blur' f b t = N . f $ Blur b [t] Nothing
blur = blur' id

chopToT :: Tree CHOP -> Tree TOP
chopToT = N <$> CHOPToTOP

circleT = circleT' id
circleT' :: (TOP -> TOP) -> Tree TOP
circleT' f = N . f $ CircleTOP

compT' :: (TOP -> TOP) -> Int -> [Tree TOP] -> Tree TOP
compT' f op ts = N . f $ CompositeTOP (int op) ts Nothing emptyV2
compT = compT' id

crop' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
crop' f = N . f <$> Crop Nothing Nothing Nothing Nothing . (:[])

displace :: Tree TOP -> Tree TOP -> Tree TOP
displace a b = N $ Displace [a, b]

edges' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
edges' f a = N . f $ Edges [a] Nothing
edges = edges' id

feedbackT :: Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP) -> Tree TOP
feedbackT = FT (FeedbackTOP Nothing [])

flipT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
flipT' f t = N . f $ Flip Nothing Nothing Nothing [t] Nothing


glslT' :: (TOP -> TOP) -> String -> [Tree TOP] -> Tree TOP
glslT' f d ts = N . f $ GLSLTOP (fileD d) [] Nothing emptyV2 ts Nothing
glslT = glslT' id

glslTP' :: (TOP -> TOP) -> String -> [(String, Vec4)] -> [Tree TOP] -> Tree TOP
glslTP' f s us ts = glslT' ((glslTUniforms .~ us) . f) s ts
glslTP = glslTP' id

hsvT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
hsvT' f = N <$> f. HSVAdjust Nothing Nothing Nothing . (:[])

levelT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
levelT' f = N <$> f. LevelTOP Nothing Nothing . (:[])

movieFileIn = movieFileIn' id
movieFileIn' :: (TOP -> TOP) -> Tree ByteString -> Tree TOP
movieFileIn' f file = N . f $ (MovieFileIn file Nothing Nothing emptyV2)

noiseT' :: (TOP -> TOP) -> Tree TOP
noiseT' f = N $ f $ NoiseTOP Nothing emptyV2 emptyV3

nullT :: Tree TOP -> Tree TOP
nullT = N . NullTOP . (:[])

outT :: Tree TOP -> Tree TOP
outT = N <$> OutTOP . (:[])

ramp' :: (TOP -> TOP) -> Tree DAT -> Tree TOP
ramp' f = N . f <$> (Ramp Nothing Nothing emptyV2)

rampC' :: (TOP -> TOP) -> [(Float, Float, Float, Float, Float)] -> Tree TOP
rampC' f = ramp' f . table . fromLists . fmap (^..each) . ((:) ("pos", "r", "g", "b", "a")) . fmap ((over each) (BS.pack . show))

render = render' id
render' :: (TOP -> TOP) -> Tree Geo -> Tree Camera -> Tree TOP
render' f geo cam = N . f $ Render geo cam Nothing

selectT :: Tree TOP -> Tree TOP
selectT = N <$> SelectTOP . Just

switchT' :: (TOP -> TOP) -> Tree Float -> [Tree TOP] -> Tree TOP
switchT' f i = N . f <$> SwitchTOP i Nothing
switchT = switchT' id

textT' :: (TOP -> TOP) -> Tree ByteString -> Tree TOP
textT' f tx = N . f $ TextTOP tx emptyV2
textT = textT' id

transformT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
transformT' f = N <$> f . (TransformTOP emptyV2 Nothing emptyV2 Nothing Nothing) . (:[])
transformT = transformT' id

vidIn :: Tree TOP
vidIn = N $ VideoDeviceIn


-- COMPs

geo' :: (Geo -> Geo) -> Tree SOP -> Tree Geo
geo' f = Comp (f $ Geo emptyV3 emptyV3 Nothing Nothing)

cam' :: (Camera -> Camera) -> Tree Camera
cam' f = N . f $ Camera emptyV3
cam = cam' id

light :: Tree Light
light = N Light

base :: (Baseable a, Baseable b) => (Tree a -> Tree b) -> Tree a -> Tree b
base = BComp $ BaseCOMP [] Nothing

tox :: (Op a, Op b) => String -> [(ByteString, Tree ByteString)] -> Maybe (Tree a) -> Tree b
tox t ps = Tox $ BaseCOMP ps (Just $ str t)
