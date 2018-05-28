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

import Control.Lens
import Data.Matrix
import Data.Maybe
import Data.Monoid

import Data.ByteString.Char8 as BS
import Data.List as L
import qualified Data.Bool as DB


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
                        , _audioFileRepeat :: Maybe (Tree Int)
                        }
          | AudioFilter { _audioFilterPass :: Tree Int
                        , _audioFilterCutoff :: Maybe (Tree Float)
                        , _chopIns :: [Tree CHOP]
                        }
          | AudioIn
          | AudioMovie { _audioMovieFileInTOP :: Tree TOP
                       }
          | AudioSpectrum { _chopIns :: [Tree CHOP] }
          | ConstantCHOP 
            { _constCValues :: [Tree Float] 
            , _constCEndFrames :: Maybe (Tree Int)
            }
          | Count { _chopIns :: [Tree CHOP]
                  , _countReset :: Maybe (Tree CHOP)
                  , _countThresh :: Maybe (Tree Float)
                  , _countLimType :: Maybe (Tree Int)
                  , _countLimMin :: Maybe (Tree Float)
                  , _countLimMax :: Maybe (Tree Float)
                  , _countResetCondition :: Maybe (Tree Int)
                  }
          | DATToCHOP { _datToCFirstCol :: Maybe (Tree Int)
                      , _datToCFirstRow :: Maybe (Tree Int)
                      , _datToCOutput :: Maybe (Tree Int)
                      , _datToCDat :: Tree DAT
                      }
          | Delay { _delayFrames :: Tree Int
                  , _chopIns :: [Tree CHOP]
                  }
          | DeleteCHOP { _deleteCNumbers :: Maybe (Tree ByteString)
                       , _deleteCNonScoped :: Maybe (Tree Bool)
                       , _chopIns :: [Tree CHOP]
                       }
          | ExpressionCHOP { _expressionCExprs :: [Tree Float]
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
                , _chopTimeSlice :: Maybe (Tree Bool)
                , _chopIns :: [Tree CHOP]
                }
          | LeapMotion { _leapMotionPinchStrength :: Maybe (Tree Bool)
                       , _leapMotionGrabStrength :: Maybe (Tree Bool)
                       }
          | Limit
            { _limitCType :: Tree Int
            , _limitCMin :: Tree Float
            , _limitCMax :: Tree Float
            , _limitCNormalize :: Maybe (Tree Bool)
            , _chopIns :: [Tree CHOP]
            }
          | Logic { _logicPreop :: Maybe (Tree Int)
                  , _logicConvert :: Maybe (Tree Int)
                  , _chopIns :: [Tree CHOP]
                  }
          | LookupCHOP
            { _chopIns :: [Tree CHOP]
            }
          | Math { _mathAdd :: Maybe (Tree Float)
                 , _mathAddPost :: Maybe (Tree Float)
                 , _mathAlign :: Maybe (Tree Int)
                 , _mathCombChops :: Maybe (Tree Int)
                 , _mathCombChans :: Maybe (Tree Int)
                 , _mathInt :: Maybe (Tree Int)
                 , _mathMult :: Maybe (Tree Float)
                 , _mathPostOp :: Maybe (Tree Int)
                 , _mathFromRange :: Vec2
                 , _mathToRange :: Vec2
                 , _chopIns :: [Tree CHOP]
                 }
          | MergeCHOP { _mergeCDupes :: Maybe (Tree Int)
                      , _mergeCAlign :: Maybe (Tree Int)
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
                      , _noiseCSeed :: Maybe (Tree Float)
                      }
          | NullCHOP { _nullCCookType :: Maybe (Tree Int)
                     , _chopIns :: [Tree CHOP]
                     }
          | OscInCHOP { _oscInCPort :: Tree ByteString
                      }
          | OutCHOP { _chopIns :: [Tree CHOP]
                    }
          | RenameCHOP { _renameCTo :: Tree ByteString
                       , _renameCFrom :: Maybe (Tree ByteString)
                       , _chopIns :: [Tree CHOP]
                       }
          | Reorder { _reorderMethod :: Tree Int
                    , _reorderSeed :: Maybe (Tree Int)
                    , _chopIns :: [Tree CHOP]
                    }
          | ReplaceCHOP { _chopIns :: [Tree CHOP]
                        }
          | ResampleCHOP { _resampleEnd :: Maybe (Tree Int)
                         , _resampleRate :: Maybe (Tree Int)
                         , _chopTimeSlice :: Maybe (Tree Bool)
                         , _chopIns :: [Tree CHOP]
                         }
          | ScriptCHOP { _scriptChopDat :: Tree DAT
                       , _chopIns :: [Tree CHOP]
                       }
          | SelectCHOP { _selectCNames :: Maybe (Tree ByteString)
                       , _selectCChop :: Maybe (Tree CHOP)
                       , _chopIns :: [Tree CHOP]
                       }
          | StretchCHOP { _stretchCEnd :: Tree Int
                        , _chopIns :: [Tree CHOP]
                        }
          | ShiftCHOP
            { _shiftCScroll :: Tree Int
            , _chopIns :: [Tree CHOP]
            }
          | ShuffleCHOP { _shuffleCMethod :: Tree Int
                        , _chopIns :: [Tree CHOP]
                        }
          | SOPToCHOP { _sopToChopSop :: Tree SOP }
          | SpeedCHOP { _chopIns :: [ Tree CHOP ] }
          | SwitchCHOP { _switchCIndex :: Tree Int
                       , _chopIns :: [Tree CHOP]
                       }
          | TOPToCHOP { _topToChopRName :: Maybe (Tree ByteString)
                      , _topToChopGName :: Maybe (Tree ByteString)
                      , _topToChopBName :: Maybe (Tree ByteString)
                      , _topToChopAName :: Maybe (Tree ByteString)
                      , _topToChopDownloadType :: Maybe (Tree Int)
                      , _topToChopCrop :: Maybe (Tree Int)
                      , _topToChopTop :: Tree TOP
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
          | TimeSlice { _chopIns :: [ Tree CHOP ] }
          | Timeline {}
          | Trail { _trailActive :: Maybe (Tree Bool)
                  , _trailWindowLengthFrames :: Maybe (Tree Int)
                  , _trailCapture :: Maybe (Tree Int)
                  , _chopIns :: [Tree CHOP]
                  }
          | WaveCHOP { _waveExprs :: Tree Float
                     , _waveEnd :: Tree Int
                     , _waveCNames :: Maybe (Tree ByteString)
                     , _waveRate :: Maybe (Tree Int)
                     }

data DAT = ChopExec { _chopExecChop :: Tree CHOP
                    , _ceOffToOn :: Maybe BS.ByteString
                    , _ceWhileOn :: Maybe BS.ByteString
                    , _ceOnToOff :: Maybe BS.ByteString
                    , _ceWhileOff :: Maybe BS.ByteString
                    , _ceValueChange :: Maybe BS.ByteString
                    }
         | CHOPToDAT { _chopToDatChop :: Tree CHOP
                     }
         | DatExec { _datExecDat :: Tree DAT
                   , _datExecTableChange :: Maybe BS.ByteString
                   , _datExecRowChange :: Maybe BS.ByteString
                   , _datVars :: [(ByteString, Tree ByteString)]
                   }
         | ExecuteDAT { _executeDatActive :: Maybe (Tree Bool)
                      , _executeDatStart :: Maybe ByteString
                      , _executeDatCreate :: Maybe ByteString
                      , _executeDatExit :: Maybe ByteString
                      , _executeDatFramestart :: Maybe ByteString
                      , _executeDatFrameend :: Maybe ByteString
                      , _datIns :: [Tree DAT]
                      , _datVars :: [(ByteString, Tree ByteString)]
                      }
         | InDAT
         | OscInDAT { _oscInDPort :: Tree ByteString
                     , _oscInDSplitBundle :: Maybe (Tree Bool)
                     , _oscInDSplitMessages :: Maybe (Tree Bool)
                     , _oscInDBundleTimestamp :: Maybe (Tree Bool)
                     , _oscInDAddressScope :: Maybe (Tree ByteString)
                     }
         | OutDAT { _datIns :: [Tree DAT]
                  }
         | ScriptDAT { _scriptDatDat :: Tree DAT
                     , _datVars :: [(ByteString, Tree ByteString)]
                     , _datIns :: [Tree DAT]
                     }
         | SelectDAT { _selectDRI :: Maybe (Tree Int)
                     , _selectDRStartI :: Maybe (Tree Int)
                     , _selectDREndI :: Maybe (Tree Int)
                     , _selectDRStartN :: Maybe (Tree ByteString)
                     , _selectDREndN :: Maybe (Tree ByteString)
                     , _selectDRExpr :: Maybe (Tree ByteString)
                     , _selectDCStartI :: Maybe (Tree Int)
                     , _selectDCEndI :: Maybe (Tree Int)
                     , _selectDCStartN :: Maybe (Tree ByteString)
                     , _selectDCEndN :: Maybe (Tree ByteString)
                     , _selectDCExpr :: Maybe (Tree ByteString)
                     , _selectDat :: Tree DAT
                     }
         | SerialDAT { _serialDBaud :: Maybe (Tree Int)
                     , _serialDPort :: String
                     , _serialDStopBits :: Maybe (Tree Int)
                     , _serialDCallbacks :: Maybe (Tree DAT)
                     }
         | Table { _tableText :: Maybe (Matrix ByteString)
                 , _tableFile :: Maybe (Tree BS.ByteString)
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

data SOP = BoxSOP { _boxSScale :: Vec3
                  }
         | CHOPToSOP { _chopToSopChop :: Tree CHOP
                     , _chopToSopAttrScope :: Maybe (Tree BS.ByteString)
                     , _chopToSChanScope :: Maybe (Tree BS.ByteString)
                     , _chopToSResample :: Maybe (Tree Bool)
                     , _sopIns :: [Tree SOP]
                     }
         | CircleSOP { _circType :: Maybe (Tree Int)
                     , _circArc :: Maybe (Tree Int)
                     , _sopIns :: [Tree SOP]
                     }
         | InSOP
         | LineSOP
         | GridSOP { _gridPrimitive :: Maybe (Tree Int)
                   , _gridRows :: Maybe (Tree Int)
                   , _gridColumns :: Maybe (Tree Int)
                   , _gridSurfType :: Maybe (Tree Int)
                   }
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
         | Torus  { _torusType :: Maybe (Tree Int)
                  , _torusRows :: Maybe (Tree Int)
                  , _torusColumns :: Maybe(Tree Int)
                  , _torusOrientation :: Maybe (Tree Int)
                  , _torusRadius :: Vec2
                  , _sopIns :: [Tree SOP]
                  }
         | TransformSOP { _transformSUniformScale :: Maybe (Tree Float)
                        , _transformSScale :: Vec3
                        , _transformSTranslate :: Vec3
                        , _sopIns :: [Tree SOP]
                        }
         | TubeSOP { _tubeRadius :: Vec2
                   , _tubeHeight :: Maybe (Tree Float)
                   }

data TOP = Blur { _blurSize :: Tree Float
                , _topIns :: [Tree TOP]
                , _topPasses :: Maybe (Tree Int)
                }
           | CHOPToTOP { _chopToTopChop :: Tree CHOP 
                       , _chopToTopFormat :: Maybe (Tree Int)
                       }
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
                      , _levelInvert :: Maybe (Tree Float)
                      , _topIns :: [Tree TOP]
                      }
           | InTOP
           | MovieFileIn { _movieFileInFile :: Tree BS.ByteString
                         , _moviePlayMode :: Maybe (Tree Int)
                         , _movieIndex :: Maybe (Tree Int)
                         , _topResolution :: IVec2
                         }
           | NdiInTOP { _ndiinName :: Tree BS.ByteString
                      }
           | NoiseTOP { _noiseTType :: Maybe (Tree Int)
                      , _noiseTMonochrome :: Maybe (Tree Bool)
                      , _noiseTResolution :: IVec2
                      , _noiseTTranslate :: Vec3
                      , _noiseTHarmonicGain :: Maybe (Tree Float)
                      , _noiseTHarmonicSpread :: Maybe (Tree Float)
                      , _noiseTExponent :: Maybe (Tree Float)
                      , _noiseTAmplitude :: Maybe (Tree Float)
                      , _noiseTOffset :: Maybe (Tree Float)
                      }
           | NullTOP { _topIns :: [Tree TOP] }
           | OutTOP { _topIns :: [Tree TOP] }
           | Ramp { _rampType :: Maybe (Tree Int)
                  , _rampPhase :: Maybe (Tree Float)
                  , _topResolution :: IVec2
                  , _rampValues :: Tree DAT
                  }
           | RectangleTOP { _rectangleSize :: Vec2
                       , _rectangleCenter :: Vec2
                       , _rectangleColor :: Vec3
                       , _rectangleBorderColor :: Vec3
                       , _rectangleBorderWidth :: Maybe (Tree Float)
                       , _topResolution :: IVec2
                       }
           | Render { _renderGeos :: [Tree Geo]
                   , _renderCamera :: Tree Camera
                   , _renderLight :: [Tree Light]
                   , _renderCullFace :: Maybe (Tree Int)
                   }
           | SelectTOP { _selectTTop :: Maybe (Tree TOP)
                       }
           | SwitchTOP { _switchTIndex :: Tree Float
                       , _switchTBlend :: Maybe (Tree Bool)
                       , _topIns :: [Tree TOP]
                       }
           | TextTOP { _textText :: Tree ByteString
                     , _textColor :: Vec3
                     , _textFontSize :: Maybe (Tree Float)
                     , _textAlign :: IVec2
                     , _topResolution :: IVec2
                     }
           | TransformTOP { _transformTranslate :: Vec2
                       , _transformExtend :: Maybe (Tree Int)
                       , _transformScale :: Vec2
                       , _transformRotate :: Maybe (Tree Float)
                       , _topPasses :: Maybe (Tree Int)
                       , _topResolution :: IVec2
                       , _topIns :: [Tree TOP]
                       }
           | VideoDeviceIn

data MAT = ConstantMAT { _constColor :: Vec3
                       , _constAlpha :: Maybe (Tree Float)
                       , _constMatMap :: Maybe (Tree TOP)
                       }
         | InMAT
         | WireframeMAT { _wireframeColor :: Vec3
                        }
         | OutMAT { _matIns :: [Tree MAT]
                  }
         | PBRMAT { _pbrBaseColorMap :: Maybe (Tree TOP)
                  , _pbrMetallic :: Maybe (Tree Float)
                  , _pbrRoughness :: Maybe (Tree Float)
                  , _pbrEmitColorMap :: Maybe (Tree TOP)
                  }

data Geo = Geo { _geoTranslate :: Vec3
                , _geoScale :: Vec3
                , _geoMat :: Maybe (Tree MAT)
                , _geoUniformScale :: Maybe (Tree Float)
                , _geoRender :: Maybe (Tree Bool)
                , _geoInstanceChop :: Maybe (Tree CHOP)
                , _geoInstanceTX :: Maybe (Tree ByteString)
                , _geoInstanceTY :: Maybe (Tree ByteString)
                , _geoInstanceTZ :: Maybe (Tree ByteString)
                , _geoInstanceRX :: Maybe (Tree ByteString)
                , _geoInstanceRY :: Maybe (Tree ByteString)
                , _geoInstanceRZ :: Maybe (Tree ByteString)
                , _geoInstanceSX :: Maybe (Tree ByteString)
                , _geoInstanceSY :: Maybe (Tree ByteString)
                , _geoInstanceSZ :: Maybe (Tree ByteString)
                }

data Camera = Camera { _camTranslate :: Vec3
                     , _camRotate :: Vec3
                     , _camPivot :: Vec3
                     , _camLookAt :: Maybe (Tree Geo)
                     }

data BaseCOMP = BaseCOMP { _baseParams :: [(ByteString, Tree ByteString)]
                         , _externalTox :: Maybe (Tree ByteString)
                         }

data Light = Light { _lightShadowType :: Maybe (Tree Int)
                   , _lightTranslate :: Vec3
                   , _lightColor :: Vec3
                   , _lightDimmer :: Maybe (Tree Float)
                   , _lightAttenuated :: Maybe (Tree Bool)
                   , _lightAttenuationStart :: Maybe (Tree Float)
                   , _lightAttenuationEnd :: Maybe (Tree Float)
                   , _lightAttenuationRolloff :: Maybe (Tree Float)
                   }

data Channel

data Tree a where
  N :: (Op a) => a -> Tree a
  FC :: CHOP -> Tree CHOP -> (Tree CHOP -> Tree CHOP) -> (Tree CHOP -> Tree CHOP) -> Tree CHOP
  FT :: TOP -> Tree TOP -> (Tree TOP -> Tree TOP) -> (Tree TOP -> Tree TOP) -> Tree TOP
  Comp :: (Op a, Op b) => a -> Tree b -> Tree a
  BComp :: (Baseable a, Baseable b) => BaseCOMP -> (Tree a -> Tree b) -> Tree a -> Tree b
  Tox :: (Op a, Op b) => BaseCOMP -> Maybe (Tree a) -> Tree b
  Fix :: (Op a) => ByteString -> Tree a -> Tree a
  PyExpr :: ByteString -> Tree a
  Mod :: (ByteString -> ByteString) -> Tree a -> Tree b
  Mod2 :: (ByteString -> ByteString -> ByteString) -> Tree a -> Tree b -> Tree c
  Mod3 :: (ByteString -> ByteString -> ByteString -> ByteString) -> Tree a -> Tree b -> Tree c -> Tree d
  Resolve :: Tree a -> Tree ByteString
  ResolveP :: Tree a -> Tree ByteString
  ResolvePS :: [Tree a] -> Tree ByteString

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

type Vec2 = (Maybe (Tree Float), Maybe (Tree Float))
type Vec3 = (Maybe (Tree Float), Maybe (Tree Float), Maybe (Tree Float))
type Vec4 = (Maybe (Tree Float), Maybe (Tree Float), Maybe (Tree Float), Maybe (Tree Float))
type IVec2 = (Maybe (Tree Int), Maybe (Tree Int))

v2 :: Tree Float -> Tree Float -> Vec2
v2 x y = (Just x, Just y)


v3 :: Tree Float -> Tree Float -> Tree Float -> Vec3
v3 x y z = (Just x, Just y, Just z)

emptyV4 = (Nothing, Nothing, Nothing, Nothing)
emptyV3 = (Nothing, Nothing, Nothing)
emptyV2 = (Nothing, Nothing)

vec2Map :: (ByteString, ByteString) -> String -> (Maybe (Tree a), Maybe (Tree a)) -> [(ByteString, Tree ByteString)]
vec2Map (x, y) n (xv, yv) = catMaybes [BS.append (pack n) x <$$> xv,  BS.append (pack n) y <$$> yv]

vec2Map' :: String -> Vec2 -> [(ByteString, Tree ByteString)]
vec2Map' = vec2Map ("x", "y")

ivec2Map' :: String -> IVec2 -> [(ByteString, Tree ByteString)]
ivec2Map' = vec2Map ("x", "y")

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

v3mult :: Tree Float -> Vec3 -> Vec3
v3mult m (x, y, z) = ((!* m) <$> x, (!* m) <$> y, (!* m) <$> z)

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
                                        , "repeat" <$$> _audioFileRepeat
                                        , "playmode" <$$> getFirst (First ( const (int 1) <$> _audioFileInIndex) <> First _audioFileInPlayMode)
                                        ]
  pars (AudioIn) = []
  pars (AudioMovie {..}) = [ ("moviefileintop", ResolveP _audioMovieFileInTOP)
                           ]
  pars (AudioSpectrum _) = []
  pars n@(ConstantCHOP {..}) = 
    L.concat (L.zipWith (\i v' -> [(BS.pack $ "value" ++ show i, Resolve v'), (BS.pack $ "name" ++ show i, str $ "chan" ++ show i)]) [0..] _constCValues) 
    ++ catMaybes [ "single" <$$> (const (int 0) <$> _constCEndFrames), "end" <$$> _constCEndFrames, "endunit" <$$> (const (int 1) <$> _constCEndFrames) ]
    ++ chopBasePars n
  pars n@(Count {..}) = catMaybes [ "threshup" <$$> _countThresh
                                  , "output" <$$> _countLimType
                                  , "limitmin" <$$> _countLimMin
                                  , "limitmax" <$$> _countLimMax
                                  , "resetcondition" <$$> _countResetCondition
                                  ] ++ chopBasePars n
  pars n@(DATToCHOP {..}) = [("dat", ResolveP _datToCDat)]
    ++ catMaybes
      [ "firstcolumn" <$$> _datToCFirstCol
      , "firstrow" <$$> _datToCFirstRow
      , "output" <$$> _datToCOutput
      ] ++ chopBasePars n
  pars n@(Delay {..}) = [("delayunit", Resolve $ int 1), ("delay", Resolve _delayFrames), ("maxdelayunit", Resolve $ int 1), ("maxdelay", Resolve _delayFrames)]
  pars n@(DeleteCHOP {..}) = catMaybes [ "selnumbers" <$$> _deleteCNumbers
                                       , ("select",) . Resolve . const (int 1) <$> _deleteCNumbers
                                       , "discard" <$$> _deleteCNonScoped
                                       ] ++ [("delchannels", Resolve (bool True))]
  pars n@(ExpressionCHOP {..}) = mconcat [ [("numexpr", Resolve . int $ L.length _expressionCExprs)]
                                         , chopBasePars n
                                         , L.zipWith (\i e -> (BS.concat ["expr", (pack . show) i], Resolve e)) [0..] _expressionCExprs
                                         ]
  pars n@(Fan o off _) = catMaybes ["fanop" <$$> o, "alloff" <$$> off] ++ chopBasePars n
  pars (FeedbackCHOP _) = []
  pars (Hold _) = []
  pars InCHOP = []
  pars n@(Lag {..}) = vec2Map ("1", "2") "lag" _lagLag ++ chopBasePars n
  pars n@(Logic p c _) = catMaybes ["preop" <$$> p, "convert" <$$> c] ++ chopBasePars n
  pars n@(LookupCHOP {..}) = chopBasePars n
  pars n@(Limit {..}) = 
    [ ("type",) . Resolve $ _limitCType
    , ("min",) . Resolve $ _limitCMin
    , ("max",) . Resolve $ _limitCMax
    ] ++ catMaybes ["norm" <$$> _limitCNormalize] ++ chopBasePars n
  pars n@(LeapMotion {..}) = catMaybes [ "pinchstrength" <$$> _leapMotionPinchStrength
                                       , "grabstrength" <$$> _leapMotionGrabStrength
                                       ] ++ chopBasePars n
  pars n@(Math {..}) = catMaybes [ "preoff" <$$> _mathAdd
                                 , "postoff" <$$> _mathAddPost
                                 , "chopop" <$$> _mathCombChops
                                 , "chanop" <$$> _mathCombChans
                                 , "integer" <$$> _mathInt
                                 , "align" <$$> _mathAlign
                                 , "gain" <$$> _mathMult
                                 , "postop" <$$> _mathPostOp
                                 ] ++ vec2Map ("1", "2") "fromrange" _mathFromRange ++ vec2Map ("1", "2") "torange" _mathToRange ++ chopBasePars n
  pars n@(MergeCHOP {..}) = catMaybes ["duplicate" <$$> _mergeCDupes, "align" <$$> _mergeCAlign] ++ chopBasePars n
  pars MidiIn = []
  pars n@(NoiseCHOP {..}) = catMaybes [ "roughness" <$$> _noiseCRoughness
                                      , "type" <$$> _noiseCType
                                      , "period" <$$> _noiseCPeriod
                                      , "amp" <$$> _noiseCAmplitude
                                      , ("channelname",) <$> _noiseCChannels
                                      , "seed" <$$> _noiseCSeed
                                      ] ++ chopBasePars n ++ vec3Map' "t" _noiseCTranslate
  pars n@(NullCHOP {..}) = catMaybes [("cooktype" <$$> _nullCCookType)] ++ chopBasePars n
  pars n@(OscInCHOP {..}) = [("port", _oscInCPort)]
  pars (OutCHOP _) = []
  pars n@(RenameCHOP {..}) = mconcat [catMaybes ["renamefrom" <$$> _renameCFrom], [("renameto", Resolve $ _renameCTo)], chopBasePars n]
  pars n@(Reorder {..}) = [("method", Resolve _reorderMethod)] ++ catMaybes ["seed" <$$> _reorderSeed] ++ chopBasePars n
  pars n@(ReplaceCHOP {..}) = chopBasePars n
  pars n@(ResampleCHOP {..}) =
    let
      method (Just _) (Just _) = 3
      method Nothing (Just _) = 0
      method _ _ = 1
    in
      catMaybes [ "rate" <$$> _resampleRate
                , "end" <$$> _resampleEnd
                ] ++ [ ("method", Resolve . int $ method _resampleRate _resampleEnd) ] ++ chopBasePars n
  pars n@(ScriptCHOP {..}) = [("callbacks", ResolveP _scriptChopDat)]
  pars n@(SelectCHOP {..}) = catMaybes [(("chop",) . ResolveP <$> _selectCChop), "channames" <$$> _selectCNames] ++ chopBasePars n
  pars n@(ShiftCHOP {..}) = [("scroll", Resolve _shiftCScroll)] ++ chopBasePars n
  pars n@(ShuffleCHOP {..}) = [("method", Resolve _shuffleCMethod)] ++ chopBasePars n
  pars n@(SpeedCHOP {..}) = chopBasePars n
  pars n@(SOPToCHOP s) = [("sop", ResolveP s)] ++ chopBasePars n
  pars n@(SwitchCHOP {..}) = [("index", Resolve _switchCIndex)] ++ chopBasePars n
  pars n@(StretchCHOP {..}) = [("end", Resolve _stretchCEnd), ("relative", Resolve $ int 0), ("endunit", Resolve $ int 1)] ++ chopBasePars n
  pars n@(TOPToCHOP {..}) = [("top", ResolveP _topToChopTop)] ++
                            catMaybes [ "downloadtype" <$$> _topToChopDownloadType
                                      , "crop" <$$> _topToChopCrop
                                      , "r" <$$> _topToChopRName
                                      , "g" <$$> _topToChopGName
                                      , "b" <$$> _topToChopBName
                                      , "a" <$$> _topToChopAName
                                      ] ++ chopBasePars n
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
  pars n@(TimeSlice {..}) = chopBasePars n
  pars n@(Timeline {}) = []
  pars n@(Trail {..}) = catMaybes [ ("active" <$$> _trailActive)
                                  , ("wlength" <$$> _trailWindowLengthFrames)
                                  , ("wlengthunit",) . Resolve . const (int 0) <$> _trailWindowLengthFrames
                                  , "capture" <$$> _trailCapture
                                  ] ++ chopBasePars n
  pars n@(WaveCHOP {..}) = mconcat [ chopBasePars n
                                   , [ ("exprs", ResolveP _waveExprs)
                                     , ("end", ResolveP _waveEnd)
                                     , ("endunit", ResolveP $ int 1)
                                     , ("wavetype", ResolveP $ int 7)
                                     ]
                                   , catMaybes [ "channelname" <$$> _waveCNames
                                               , "rate" <$$> _waveRate
                                               ]
                                   ]
  opType (Analyze {}) = "analyze"
  opType (AudioDeviceOut {}) = "audioDevOut"
  opType (AudioFileIn {}) = "audioFileIn"
  opType (AudioMovie {}) = "audioMovie"
  opType (AudioFilter {}) = "audioFilter"
  opType (AudioIn {}) = "audioIn"
  opType (AudioSpectrum {}) = "audioSpectrum"
  opType (ConstantCHOP {}) = "constantChop"
  opType (Count {}) = "count"
  opType (DATToCHOP {}) = "datToChop"
  opType (Delay {}) = "delay"
  opType (DeleteCHOP {}) = "deleteChop"
  opType (ExpressionCHOP {}) = "expressionChop"
  opType (Fan {}) = "fan"
  opType (FeedbackCHOP _) = "feedbackChop"
  opType (Hold {}) = "hold"
  opType (InCHOP {}) = "inChop"
  opType (Lag {}) = "lag"
  opType (Limit {}) = "limitChop"
  opType (Logic {}) = "logic"
  opType (LookupCHOP {}) = "lookupChop"
  opType (LeapMotion {}) = "leapmotion"
  opType (Math {}) = "math"
  opType (MergeCHOP {}) = "mergeChop"
  opType (MidiIn {}) = "midiIn"
  opType (NoiseCHOP {}) = "noiseChop"
  opType (NullCHOP {}) = "nullChop"
  opType (OscInCHOP {}) = "oscInChop"
  opType (OutCHOP {}) = "outChop"
  opType (SwitchCHOP {}) = "switchChop"
  opType (StretchCHOP {}) = "stretchChop"
  opType (RenameCHOP {}) = "renameChop"
  opType (Reorder {}) = "reorderChop"
  opType (ReplaceCHOP {}) = "replaceChop"
  opType (ResampleCHOP {}) = "resampleChop"
  opType (ScriptCHOP {}) = "scriptChop"
  opType (SelectCHOP {}) = "selectChop"
  opType (ShiftCHOP {}) = "shiftChop"
  opType (ShuffleCHOP {}) = "shuffleChop"
  opType (SOPToCHOP _) = "sopToChop"
  opType (SpeedCHOP {}) = "speedChop"
  opType (TOPToCHOP {}) = "topToChop"
  opType (Timer {}) = "timer"
  opType (TimeSlice {}) = "timesliceChop"
  opType (Timeline {}) = "timelineChop"
  opType (Trail {}) = "trailChop"
  opType (WaveCHOP {}) = "waveChop"
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
  pars (CHOPToDAT {..}) = [("chop", ResolveP _chopToDatChop)]
  pars (DatExec {..}) = ("dat", ResolveP _datExecDat):(catMaybes 
    [ ("tablechange",) . Resolve . LambdaDesigner.Op.bool . const True <$> _datExecTableChange
    , ("rowchange",) . Resolve . LambdaDesigner.Op.bool . const True <$> _datExecRowChange
    ])
  pars (ExecuteDAT {..}) = catMaybes [ "active" <$$> _executeDatActive
                                     , "start" <$$> (LambdaDesigner.Op.bool . const True <$> _executeDatStart)
                                     , "create" <$$> (LambdaDesigner.Op.bool . const True <$> _executeDatCreate)
                                     , "exit" <$$> (LambdaDesigner.Op.bool . const True <$> _executeDatExit)
                                     , "framestart" <$$> (LambdaDesigner.Op.bool . const True <$> _executeDatFramestart)
                                     , "frameend" <$$> (LambdaDesigner.Op.bool . const True <$> _executeDatFrameend)
                                     ]
  pars n@(OscInDAT {..}) = ("port", _oscInDPort):(catMaybes 
    [ "splitbundle" <$$> _oscInDSplitBundle
    , "splitmessage" <$$> _oscInDSplitMessages
    , "bundletimestamp" <$$> _oscInDBundleTimestamp
    , "addscope" <$$> _oscInDAddressScope
    ])
  pars (ScriptDAT {..}) = [("callbacks", ResolveP _scriptDatDat)]
  pars (SelectDAT {..}) = maybe altChoice (\row -> [("rowindexstart", Resolve row), ("rowindexend", Resolve row), ("extractrows", Resolve $ int 2)]) _selectDRI ++ [("dat", ResolveP _selectDat)]
                          where
                            altChoice = catMaybes 
                                    [ ("rownamestart" <$$> _selectDRStartN)
                                    , ("rowindexstart" <$$> _selectDRStartI)
                                    , ("rownameend" <$$> _selectDREndN)
                                    , ("rowindexend" <$$> _selectDREndI)
                                    , ("rowexpr" <$$> _selectDRExpr)
                                    , ("colnamestart" <$$> _selectDCStartN)
                                    , ("colindexstart" <$$> _selectDCStartI)
                                    , ("colnameend" <$$> _selectDCEndN)
                                    , ("colindexend" <$$> _selectDCEndI)
                                    , ("colexpr" <$$> _selectDCExpr)
                                    ] ++ 
                                    [ ("extractrows", Resolve . int $ chooseType _selectDRExpr _selectDRStartN _selectDRStartI _selectDREndN _selectDREndI)
                                    , ("extractcols", Resolve . int $ chooseType _selectDCExpr _selectDCStartN _selectDCStartI _selectDCEndN _selectDCEndI)
                                    ]
                            chooseType (Just _) _ _ _ _ = 6
                            chooseType _ (Just _) Nothing (Just _) Nothing = 1
                            chooseType _ Nothing (Just _) Nothing (Just _) = 2
                            chooseType _ (Just _) Nothing Nothing (Just _) = 3
                            chooseType _ Nothing (Just _) (Just _) Nothing = 4
                            chooseType _ _ _ _ _ = 0
  pars (SerialDAT {..}) =
    [ ("port", Resolve $ str _serialDPort) ]
    ++ catMaybes
      [ "baudrate" <$$> _serialDBaud
      , "stopbits" <$$> _serialDStopBits
      , ("callbacks",) . ResolveP <$> _serialDCallbacks
      ]
  pars (TextDAT {..}) = catMaybes [("file" <$$> _textFile)]
  pars (Table {..}) = catMaybes [("file" <$$> _tableFile)]
  pars (TCPIPDAT m d f _) = ("callbacks", ResolveP d):(catMaybes [("mode" <$$> m), ("format" <$$> f)])
  pars _ = []
  opType (ChopExec _ _ _ _ _ _) = "chopExec"
  opType (CHOPToDAT {}) = "chopToDat"
  opType (DatExec {}) = "datExec"
  opType (ExecuteDAT {}) = "executeDat"
  opType (InDAT {}) = "inDat"
  opType (OscInDAT {}) = "oscInDat"
  opType (OutDAT {}) = "outDat"
  opType (ScriptDAT {}) = "scriptDat"
  opType (SelectDAT {}) = "selectDat"
  opType (SerialDAT {}) = "serialDat"
  opType (TextDAT {}) = "textDat"
  opType (Table {}) = "table"
  opType (TCPIPDAT _ _ _ _) = "tcpip"
  text (Table t _) = BS.intercalate ("\n") . fmap (BS.intercalate ("\t")) . toLists <$> t
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
  text (DatExec {..}) = Just . BS.intercalate "\n\n" $ catMaybes [ BS.append "def tableChange(dat):\n" <$> _datExecTableChange, BS.append "def rowChange(dat, rows):\n" <$> _datExecRowChange]
  text (ExecuteDAT {..}) =
    Just . BS.intercalate "\n\n" $
      ((traverse %~ concatFunc make_) $ catMaybes [ ("onStart",) <$> _executeDatStart
                  , ("create",) <$> _executeDatCreate
                  , ("onExit",) <$> _executeDatExit
                  ]) ++
      ((traverse %~ concatFunc makef) $ catMaybes [
                  ("onFrameStart",) <$> _executeDatFramestart
                  , ("onFrameEnd",) <$> _executeDatFrameend
                  ])
    where
      concatFunc f (name, body) = BS.append (f name) body
      makef prog = BS.concat ["def ", prog, "(frame):\n\t"]
      make_ prog = BS.concat ["def ", prog, "():\n\t"]
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
  pars WireframeMAT {..}= rgbMap "color" _wireframeColor
  pars PBRMAT {..}= catMaybes [("basecolormap",) . ResolveP <$> _pbrBaseColorMap, "metallic" <$$> _pbrMetallic, "roughness" <$$> _pbrRoughness, ("emitmap",) . ResolveP <$> _pbrEmitColorMap] ++ fromMaybe [] ((\_ -> rgbMap "emit" (v3 (float 1) (float 1) (float 1))) <$> _pbrEmitColorMap)
  pars (OutMAT _) = []
  opType (ConstantMAT {}) = "constMat"
  opType InMAT = "inMat"
  opType WireframeMAT {} = "wireframeMat"
  opType (OutMAT _) = "outMat"
  opType (PBRMAT {}) = "pbrMat"
  connections (maybeToList . flip (^?) matIns -> cs) = mconcat cs

instance Baseable MAT where
  inOp = N $ InMAT
  outOp o = N $ OutMAT [o]

instance Op SOP where
  pars (BoxSOP {..}) = vec3Map' "size" _boxSScale
  pars (CircleSOP p a _) = catMaybes [ ("type" <$$> p) , ("arc" <$$> a)]
  pars (GridSOP {..}) = catMaybes [ "type" <$$> _gridPrimitive
                                  , "rows" <$$> _gridRows
                                  , "cols" <$$> _gridColumns
                                  , "surftype" <$$> _gridSurfType
                                  ]
  pars (CHOPToSOP {..}) = ("chop", ResolveP _chopToSopChop):(catMaybes [ ("chanscope" <$$> _chopToSChanScope)
                                                                       , ("attscope" <$$> _chopToSopAttrScope)
                                                                       , ("mapping" <$$> _chopToSResample)
                                                                       ])
  pars (Sphere p _) = catMaybes [ ("type" <$$> p) ]
  pars (Torus {..}) = mconcat [ catMaybes ["type" <$$> _torusType, "orient" <$$> _torusOrientation, "rows" <$$> _torusRows, "cols" <$$> _torusColumns]
                                 , vec2Map' "rad" _torusRadius
                                 ]
  pars (Metaball {..}) = vec3Map' "rad" _metaballRadius ++ vec3Map' "t" _metaballCenter
  pars (NoiseSOP t _) = vec3Map' "t" t
  pars (TransformSOP {..}) = catMaybes ["scale" <$$> _transformSUniformScale]
                             ++ vec3Map' "t" _transformSTranslate ++ vec3Map' "s" _transformSScale
  pars (TubeSOP {..}) = catMaybes ["height" <$$> _tubeHeight] ++ vec2Map ("1", "2") "rad" _tubeRadius
  pars _ = []
  opType (BoxSOP {}) = "boxSop"
  opType (CHOPToSOP {}) = "chopToSop"
  opType (CircleSOP {}) = "circleSop"
  opType (GridSOP {}) = "gridSop"
  opType (InSOP {}) = "inSop"
  opType (LineSOP {}) = "lineSop"
  opType (MergeSOP {}) = "mergeSop"
  opType (Metaball {}) = "metaball"
  opType (NoiseSOP {}) = "noiseSop"
  opType (OutSOP {}) = "outSop"
  opType (Sphere {}) = "sphere"
  opType (Torus {}) = "torusSop"
  opType (TransformSOP {}) = "transformSop"
  opType (TubeSOP {}) = "tubeSop"
  connections (maybeToList . flip (^?) sopIns -> cs) = mconcat cs

instance Baseable SOP where
  inOp = N $ InSOP
  outOp o = N $ OutSOP [o]

instance Op TOP where
  pars t@(Blur {..}) = [("size",) . Resolve $ _blurSize] ++ topBasePars t
  pars (CHOPToTOP {..}) = [("chop", ResolveP _chopToTopChop)] ++ catMaybes ["dataformat" <$$> _chopToTopFormat]
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
  pars (LevelTOP {..}) = catMaybes [("opacity" <$$> _levelOpacity), "brightness1" <$$> _levelBrightness, "invert" <$$> _levelInvert]
  pars n@(NdiInTOP {..}) = [ ("name", Resolve _ndiinName) ]
  pars (NoiseTOP {..}) = (catMaybes [ ("mono" <$$> _noiseTMonochrome)
                                     , ("type" <$$> _noiseTType)
                                     , ("gain" <$$> _noiseTHarmonicGain)
                                     , ("spread" <$$> _noiseTHarmonicSpread)
                                     , ("exp" <$$> _noiseTExponent)
                                     , ("amp" <$$> _noiseTAmplitude)
                                     , ("offset" <$$> _noiseTOffset)
                                     ]) 
                                     ++ (dimenMap "resolution" _noiseTResolution) 
                                     ++ vec3Map' "t" _noiseTTranslate
  pars (SwitchTOP {..}) = [("index", Resolve _switchTIndex)] ++ catMaybes ["blend" <$$> _switchTBlend]
  pars (Ramp t p r dat) = ("dat", ResolveP dat):(dimenMap "resolution" r) ++ (catMaybes [("type" <$$>  t), ("phase" <$$> p)])
  pars t@(RectangleTOP {..}) =  vec2Map' "size" _rectangleSize ++
                                vec2Map' "center" _rectangleCenter ++
                                rgbMap "fillcolor" _rectangleColor ++
                                rgbMap "border" _rectangleBorderColor ++
                                catMaybes [ "borderwidth" <$$> _rectangleBorderWidth ] ++ topBasePars t
  pars (Render {..}) =  [ ("geometry", ResolvePS _renderGeos)
                        , ("camera", ResolveP _renderCamera)
                        , ("lights", ResolvePS _renderLight)] ++
                        catMaybes ["cullface" <$$> _renderCullFace]
  pars (SelectTOP c) = catMaybes [("top",) . ResolveP <$> c]
  pars t@(TextTOP {..}) = [("text", _textText)]
    ++ rgbMap "fontcolor" _textColor
    ++ ivec2Map' "align" _textAlign
    ++ catMaybes ["fontsizex" <$$> _textFontSize]
    ++ topBasePars t
  pars t@(TransformTOP {..}) = vec2Map' "t" _transformTranslate ++ vec2Map' "s" _transformScale ++
    catMaybes [ "rotate" <$$> _transformRotate
              , "extend" <$$> _transformExtend
              ] ++ topBasePars t
  pars _ = []


  opType (Blur {}) = "blur"
  opType (CHOPToTOP {}) = "chopToTop"
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
  opType (NdiInTOP {}) = "ndiinTop"
  opType (NoiseTOP {}) = "noiseTop"
  opType (NullTOP {}) = "nullTop"
  opType (OutTOP {})= "outTop"
  opType (Ramp _ _ _ _) = "ramp"
  opType (Render {}) = "render"
  opType (RectangleTOP {}) = "rectangleTop"
  opType (SelectTOP _) = "selectTop"
  opType (SwitchTOP {}) = "switchTop"
  opType (TextTOP {}) = "textTop"
  opType (TransformTOP {}) = "transform"
  opType (VideoDeviceIn) = "videoDeviceIn"
  connections (maybeToList . flip (^?) topIns -> cs) = mconcat cs

topBasePars :: TOP -> [(ByteString, (Tree ByteString))]
topBasePars c =  catMaybes [ "resolutionw" <$$> (c ^? topResolution._1._Just)
                           , "resolutionh" <$$> (c ^? topResolution._2._Just)
                           , "format" <$$> (c ^? pixelFormat._Just)
                           , "npasses" <$$> (c ^? topPasses._Just)
                           , ("outputresolution",) <$> (fmap (const (Resolve $ int 9)) $ safeHead $
                               catMaybes [c ^? topResolution._1._Just, c ^? topResolution._2._Just])
                           , ("outputaspect",) <$> (fmap (const (Resolve $ int 1)) $ safeHead $
                               catMaybes [c ^? topResolution._1._Just, c ^? topResolution._2._Just])
                           ]
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

instance Baseable TOP where
  inOp = N $ InTOP
  outOp o = N $ OutTOP [o]

instance Op Geo where
  opType (Geo {}) = "geo"
  pars (Geo {..}) = mconcat [ catMaybes [ ("material",) . ResolveP <$> _geoMat
                                        , ("scale" <$$> _geoUniformScale)
                                        , "render" <$$> _geoRender
                                        , "instancetx" <$$> _geoInstanceTX
                                        , "instancety" <$$> _geoInstanceTY
                                        , "instancetz" <$$> _geoInstanceTZ
                                        , "instancerx" <$$> _geoInstanceRX
                                        , "instancery" <$$> _geoInstanceRY
                                        , "instancerz" <$$> _geoInstanceRZ
                                        , "instancesx" <$$> _geoInstanceSX
                                        , "instancesy" <$$> _geoInstanceSY
                                        , "instancesz" <$$> _geoInstanceSZ
                                        ]
                            , (vec3Map' "t" _geoTranslate)
                            , (vec3Map' "s" _geoScale)
                            , maybe [] (\ic -> [("instanceop", ResolveP ic), ("instancing", Resolve $ bool True)]) _geoInstanceChop
                            ]

instance Op Camera where
  opType (Camera {}) = "camera"
  pars (Camera {..}) = vec3Map' "t" _camTranslate ++ vec3Map' "r" _camRotate ++ vec3Map' "p" _camPivot ++
    catMaybes [("lookat",) . ResolveP <$> _camLookAt]

instance Op Light where
  pars (Light {..}) = catMaybes [ "shadowtype" <$$> _lightShadowType
                                , "dimmer" <$$> _lightDimmer
                                , "attenuated" <$$> _lightAttenuated
                                , "attenuationstart" <$$> _lightAttenuationStart
                                , "attenuationend" <$$> _lightAttenuationEnd
                                , "attenuationexp" <$$> _lightAttenuationRolloff
                                ] ++ rgbMap "c" _lightColor ++ vec3Map' "t" _lightTranslate
  opType Light {} = "light"

instance Op BaseCOMP where
  pars (BaseCOMP {..}) = catMaybes ["externaltox" <$$> _externalTox]
  customPars (BaseCOMP {..}) = _baseParams
  opType (BaseCOMP {}) = "base"
  commands (BaseCOMP {..}) = []

-- CHOPs

analyze :: Tree Int -> Tree CHOP -> Tree CHOP
analyze f c = N $ Analyze f [c]


audioDevOut' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
audioDevOut' f = N <$> f . AudioDeviceOut Nothing . (:[])
audioDevOut = audioDevOut' id

audioFileIn' :: (CHOP -> CHOP) -> Tree ByteString -> Tree CHOP
audioFileIn' f file = N . f $ (AudioFileIn file Nothing Nothing Nothing Nothing)
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

constC' :: (CHOP -> CHOP) -> [Tree Float] -> Tree CHOP
constC' f fs = N . f $ ConstantCHOP fs Nothing
constC = constC' id

count' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
count' f t = N ins
  where
    def = f $ Count [t] Nothing Nothing Nothing Nothing Nothing Nothing
    ins = def & chopIns %~ (flip (++) (catMaybes . maybeToList $ def ^? countReset))
count = count' id

datToC' :: (CHOP -> CHOP) -> Tree DAT -> Tree CHOP
datToC' f = N . f <$> DATToCHOP Nothing Nothing Nothing
datToC = datToC' id


delay :: Tree Int -> Tree CHOP -> Tree CHOP
delay f = N <$> Delay f . (:[])

deleteCNum' :: (CHOP -> CHOP) -> Tree ByteString -> Tree CHOP -> Tree CHOP
deleteCNum' f d = N . f <$> DeleteCHOP (Just d) Nothing . (:[])


expressionC :: [Tree Float] -> [Tree CHOP] -> Tree CHOP
expressionC es is = N $ ExpressionCHOP es is

feedbackC :: Tree CHOP -> (Tree CHOP -> Tree CHOP) -> (Tree CHOP -> Tree CHOP) -> Tree CHOP
feedbackC = FC (FeedbackCHOP [])


fan' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
fan' f = N <$> f . Fan Nothing Nothing . (:[])
fan = fan' id

hold' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP -> Tree CHOP
hold' f h t = N <$> f $ Hold [h, t]
hold = hold' id

lag' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
lag' f = N . f <$> Lag emptyV2 Nothing . (:[])
lag :: Tree Float -> Tree Float -> Tree CHOP -> Tree CHOP
lag a b t = N $ Lag (Just a, Just b) Nothing [t]

leapmotion' :: (CHOP -> CHOP) -> Tree CHOP
leapmotion' f = N . f $ LeapMotion Nothing Nothing
leapmotion = leapmotion' id

limitC' :: (CHOP -> CHOP) -> Tree Int -> Tree Float -> Tree Float -> Tree CHOP -> Tree CHOP
limitC' f t min max = N <$> f . Limit t min max Nothing . (:[])
limitC = limitC' id

logic' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
logic' f = N <$> f . Logic Nothing Nothing
logic = logic' id

lookupC :: Tree CHOP -> Tree CHOP -> Tree CHOP
lookupC a b = N $ LookupCHOP [a, b]

math' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
math' f = N <$> f . Math Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing emptyV2 emptyV2


mergeC' :: (CHOP -> CHOP) -> [Tree CHOP] -> Tree CHOP
mergeC' f = N . f <$> MergeCHOP Nothing Nothing
mergeC = mergeC' id

mchan :: String -> Tree Float
mchan s = chanNamef s $ N MidiIn

mselect :: String -> Tree CHOP
mselect s = selectC' (selectCNames ?~ str s) $ N MidiIn

noiseC' :: (CHOP -> CHOP) -> Tree CHOP
noiseC' f = N (f $ NoiseCHOP Nothing emptyV3 Nothing Nothing Nothing Nothing Nothing Nothing)
noiseC = noiseC' id

nullC' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
nullC' f = N <$> f . NullCHOP Nothing . (:[])
nullC = nullC' id
cookC = nullC' (nullCCookType ?~ int 1)

oscinC :: Int -> Tree CHOP
oscinC p = N $ OscInCHOP (Resolve $ int p)


opsadd :: CHOP -> CHOP
opsadd = mathCombChops .~ Just (int 1)

opaddf :: Float -> CHOP -> CHOP
opaddf a = mathAdd .~ Just (float a)


opmultf :: Float -> CHOP -> CHOP
opmultf a = mathMult .~ Just (float a)

outC :: Tree CHOP -> Tree CHOP
outC = N <$> OutCHOP . (:[])

renameC' :: (CHOP -> CHOP) -> Tree ByteString -> Tree CHOP -> Tree CHOP
renameC' f newName = N . f <$> RenameCHOP newName Nothing . (:[])
renameC = renameC' id

reorderC' :: (CHOP -> CHOP) -> Tree Int -> Tree CHOP -> Tree CHOP
reorderC' f m = N . f <$> Reorder m Nothing . (:[])

replaceC :: [Tree CHOP] -> Tree CHOP
replaceC = N <$> ReplaceCHOP

resampleC' :: (CHOP -> CHOP) -> Bool -> Tree CHOP -> Tree CHOP
resampleC' f tc = N . f <$> ResampleCHOP Nothing Nothing (Just $ bool tc) . (:[])

scriptC :: String -> [Tree CHOP] -> Tree CHOP
scriptC file = N <$> ScriptCHOP (fileD file)

scriptCDAT :: Tree DAT -> [Tree CHOP] -> Tree CHOP
scriptCDAT dat = N <$> ScriptCHOP dat

selectC' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
selectC' f c = N . f $ SelectCHOP Nothing (Just c) []
selectC = selectC' id

selectCConnect' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
selectCConnect' f = N . f <$> SelectCHOP Nothing Nothing . (:[])

shiftC :: Tree Int -> Tree CHOP -> Tree CHOP
shiftC s = N <$> ShiftCHOP s . (:[])

shuffleC :: Tree Int -> Tree CHOP -> Tree CHOP
shuffleC s = N <$> ShuffleCHOP s . (:[])

sopToC :: Tree SOP -> Tree CHOP
sopToC = N <$> SOPToCHOP


speedC :: Tree CHOP -> Maybe (Tree CHOP) -> Tree CHOP
speedC s r = N $ SpeedCHOP (s:(catMaybes [r]))

stretchC :: Tree Int -> Tree CHOP -> Tree CHOP
stretchC i = N <$> StretchCHOP i . (:[])


switchC :: Tree Int -> [Tree CHOP] -> Tree CHOP
switchC i = N <$> SwitchCHOP i

topToC' :: (CHOP -> CHOP) -> Tree TOP -> Tree CHOP
topToC' f = N . f <$> TOPToCHOP Nothing Nothing Nothing Nothing Nothing Nothing
topToC = topToC' id

trailC' :: (CHOP -> CHOP) -> Tree CHOP -> Tree CHOP
trailC' f i = N . f $ Trail Nothing Nothing Nothing [i]

waveC' :: (CHOP -> CHOP) -> Tree Int -> Tree Float -> Tree CHOP
waveC' f e ex = N . f $ WaveCHOP ex e Nothing Nothing
waveC = waveC' id

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

timeslice :: Tree CHOP -> Tree CHOP
timeslice c = N $ TimeSlice [c]

timeline :: Tree CHOP
timeline = N Timeline

-- DATs

arduino :: String -> Int -> Tree DAT
arduino p b = N $ SerialDAT (Just $ int b) p (Just $ int 0) Nothing

cell :: (Integral a, Integral b) => (Tree a, Tree b) -> Tree DAT -> Tree BS.ByteString
cell (r, c) = Mod3 (\r' c' d -> BS.concat [d, "[", r', ",", c', "]"]) r c

chopExec' :: (DAT -> DAT) -> Tree CHOP -> Tree DAT
chopExec' f chop = N $ f $ ChopExec chop Nothing Nothing Nothing Nothing Nothing

chopToD :: Tree CHOP -> Tree DAT
chopToD c = N $ CHOPToDAT c

datExec' :: (DAT -> DAT) -> Tree DAT -> Tree DAT
datExec' f d = N $ f $ DatExec d Nothing Nothing []

executeD' :: (DAT -> DAT) -> [Tree DAT] -> Tree DAT
executeD' f d = N $ f $ ExecuteDAT Nothing Nothing Nothing Nothing Nothing Nothing d []

fileD' :: (DAT -> DAT) -> String -> Tree DAT
fileD' f file = N . f $ (TextDAT Nothing (Just . PyExpr $ BS.pack ("\"" ++ file ++ "\"")) [])
fileD = fileD' id

oscinD' :: (DAT -> DAT) -> Int -> Tree DAT
oscinD' f p = N . f $ OscInDAT (Resolve $ int p) (Just $ bool True) (Just $ bool True) (Just $ bool True) Nothing
oscinD = oscinD' id

scriptD' :: (DAT -> DAT) -> String -> [Tree DAT] -> Tree DAT
scriptD' f file = N . f <$> ScriptDAT (fileD file) []
scriptD = scriptD' id

selectD' :: (DAT -> DAT) -> Tree DAT -> Tree DAT
selectD' f t = N . f $ SelectDAT Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing t

table :: Matrix BS.ByteString -> Tree DAT
table t = N $ Table (Just t) Nothing

tableF :: String -> Tree DAT
tableF f = N $ Table Nothing (Just $ str f)

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

wireframeM' :: (MAT -> MAT) -> Tree MAT
wireframeM' f = N . f $ WireframeMAT emptyV3
wireframeM = wireframeM' id

pbrM' :: (MAT -> MAT) -> Tree MAT
pbrM' f = N . f $ PBRMAT Nothing Nothing Nothing Nothing


-- SOPs

boxS' :: (SOP -> SOP) -> Tree SOP
boxS' f = N . f $ BoxSOP emptyV3

chopToS' :: (SOP -> SOP) -> Tree CHOP -> Maybe (Tree SOP) -> Tree SOP
chopToS' f c i = N . f $ CHOPToSOP c Nothing Nothing Nothing (maybeToList i)
chopToS = chopToS' id

circleS' :: (SOP -> SOP) -> Tree SOP
circleS' f = N . f $ CircleSOP Nothing Nothing []
circleS = circleS' id

gridS' :: (SOP -> SOP) -> Tree SOP
gridS' f = N . f $ GridSOP Nothing Nothing Nothing Nothing
gridS = gridS' id

lineS :: Tree SOP
lineS = N $ LineSOP

mergeS :: [Tree SOP] -> Tree SOP
mergeS = N . MergeSOP

metaball' :: (SOP -> SOP) -> Tree SOP
metaball' f = N . f $ Metaball emptyV3 emptyV3

noiseS' :: (SOP -> SOP) -> Tree SOP -> Tree SOP
noiseS' f = N <$> f . NoiseSOP emptyV3 . (:[])
noiseS = noiseS' id

outS :: Tree SOP -> Tree SOP
outS = N <$> OutSOP . (:[])

scaleS :: Tree Float -> Tree SOP -> Tree SOP
scaleS f s = transformS' (transformSUniformScale ?~ f) s

sphere' :: (SOP -> SOP) -> Tree SOP
sphere' f = N . f $ Sphere Nothing []
sphere = sphere' id

torus' :: (SOP -> SOP) -> Tree SOP
torus' f = N . f $ Torus Nothing Nothing Nothing Nothing emptyV2 []
torus = torus' id

transformS' :: (SOP -> SOP) -> Tree SOP -> Tree SOP
transformS' f = N <$> f . (TransformSOP Nothing emptyV3 emptyV3) . (:[])
transformS = transformS' id

tubeS' :: (SOP -> SOP) -> Tree SOP
tubeS' f = N . f $ TubeSOP emptyV2 Nothing

-- TOPs

blur' :: (TOP -> TOP) -> Tree Float -> Tree TOP -> Tree TOP
blur' f b t = N . f $ Blur b [t] Nothing
blur = blur' id

chopToT' :: (TOP -> TOP) -> Tree CHOP -> Tree TOP
chopToT' f c = N . f $ CHOPToTOP c Nothing
chopToT = chopToT' id

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
levelT' f = N <$> f. LevelTOP Nothing Nothing Nothing . (:[])

movieFileIn = movieFileIn' id
movieFileIn' :: (TOP -> TOP) -> Tree ByteString -> Tree TOP
movieFileIn' f file = N . f $ (MovieFileIn file Nothing Nothing emptyV2)

ndiinT :: String -> Tree TOP
ndiinT n = N $ (NdiInTOP (str n))


noiseT' :: (TOP -> TOP) -> Tree TOP
noiseT' f = N $ f $ NoiseTOP Nothing Nothing emptyV2 emptyV3 Nothing Nothing Nothing Nothing Nothing

nullT :: Tree TOP -> Tree TOP
nullT = N . NullTOP . (:[])

outT :: Tree TOP -> Tree TOP
outT = N <$> OutTOP . (:[])

ramp' :: (TOP -> TOP) -> Tree DAT -> Tree TOP
ramp' f = N . f <$> (Ramp Nothing Nothing emptyV2)

rampC' :: (TOP -> TOP) -> [(Float, Float, Float, Float, Float)] -> Tree TOP
rampC' f = ramp' f . table . fromLists . fmap (^..each) . ((:) ("pos", "r", "g", "b", "a")) . fmap ((over each) (BS.pack . show))

rectangle' :: (TOP -> TOP) -> Vec2 -> Tree TOP
rectangle' f size = N . f $ RectangleTOP size emptyV2 emptyV3 emptyV3 Nothing emptyV2
rectangle = rectangle' id

render = render' id
render' :: (TOP -> TOP) -> [Tree Geo] -> Tree Camera -> Tree TOP
render' f geos cam = N . f $ Render geos cam [] Nothing

selectT :: Tree TOP -> Tree TOP
selectT = N <$> SelectTOP . Just

switchT' :: (TOP -> TOP) -> Tree Float -> [Tree TOP] -> Tree TOP
switchT' f i = N . f <$> SwitchTOP i Nothing
switchT = switchT' id

textT' :: (TOP -> TOP) -> Tree ByteString -> Tree TOP
textT' f tx = N . f $ TextTOP tx emptyV3 Nothing emptyV2 emptyV2
textT = textT' id

transformT' :: (TOP -> TOP) -> Tree TOP -> Tree TOP
transformT' f = N <$> f . (TransformTOP emptyV2 Nothing emptyV2 Nothing Nothing emptyV2) . (:[])
transformT = transformT' id

vidIn :: Tree TOP
vidIn = N $ VideoDeviceIn


-- COMPs

geo' :: (Geo -> Geo) -> Tree SOP -> Tree Geo
geo' f = Comp (f $ Geo emptyV3 emptyV3 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

instanceGeo' :: (Geo -> Geo) -> Tree CHOP -> Tree SOP -> Tree Geo
instanceGeo' f c = geo' (f
                         . (geoInstanceTX ?~ str "tx") . (geoInstanceTY ?~ str "ty") . (geoInstanceTZ ?~ str "tz")
                         . (geoInstanceRX ?~ str "rx") . (geoInstanceRY ?~ str "ry") . (geoInstanceRZ ?~ str "rz")
                         . (geoInstanceSX ?~ str "sx") . (geoInstanceSY ?~ str "sy") . (geoInstanceSZ ?~ str "sz")
                         . (geoInstanceChop ?~ c))

cam' :: (Camera -> Camera) -> Tree Camera
cam' f = N . f $ Camera emptyV3 emptyV3 emptyV3 Nothing
cam = cam' id

light' :: (Light -> Light) -> Tree Light
light' f = N . f $ Light Nothing emptyV3 emptyV3 Nothing Nothing Nothing Nothing Nothing
light = light' id

base :: (Baseable a, Baseable b) => (Tree a -> Tree b) -> Tree a -> Tree b
base = BComp $ BaseCOMP [] Nothing

tox :: (Op a, Op b) => String -> [(ByteString, Tree ByteString)] -> Maybe (Tree a) -> Tree b
tox t ps = Tox $ BaseCOMP ps (Just $ str t)
