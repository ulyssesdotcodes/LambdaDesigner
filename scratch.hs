{-# LANGUAGE OverloadedStrings #-}

import LambdaDesigner.Op
import LambdaDesigner.Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.Char
import Data.IORef
import Data.Matrix
import Data.Maybe
import Data.List as L

import qualified Data.ByteString.Char8 as BS

go =
  let
    step = 0.1
    steps = [0, step .. 1]
    ain = math' (mathMult ?~ float 1) [audioIn]
    volume = chan0f $ analyze (int 6) ain
    band = chan0f . analyze (int 6) . flip bandPass ain
    ltranslate f = float (f * 3) !- float 2
    lightgeo f = geo' ((geoRender ?~ band (float f) !> float 0.05) . (geoTranslate._1 ?~ ltranslate f) . (geoUniformScale ?~ float (step * 0.5)) . (geoMat ?~ lmat (float f))) (outS $ tubeS' (tubeHeight ?~ float 10))
    lrender = render' id (lightgeo <$> steps) cam & hsvT' (hsvAdjValMult ?~ float 1.3)
    palettecolor = croppalette purplish
    chopcol l n = palettecolor (float l !+ (seconds !* float 0.3)) & topToC & chanf n
    lights l = light' ((lightColor .~ v3 (chopcol l 0) (chopcol l 1) (chopcol l 2))
                       . (lightTranslate._1 ?~ ltranslate l)
                       . (lightDimmer ?~ float 16 !* castf (band (float l) !> float 0.05))
                       . (lightAttenuated ?~ bool True)
                       . (lightAttenuationStart ?~ float 2)
                       . (lightAttenuationEnd ?~ float 10.3)
                       . (lightAttenuationRolloff ?~ float 0.4)
                      )
    lmat l = pbrM' (pbrEmitColorMap ?~ (palettecolor (l !+ (seconds !* float 0.3))))
    wallgeo = geo' ((geoMat ?~ pbrM' ((pbrMetallic ?~ float 0) . (pbrBaseColorMap ?~ movieFileIn (str "C:/Users/ulyssesp/Downloads/concrete_bare_2159_2638_Small.jpg")))) . (geoUniformScale ?~ float 8) . (geoTranslate._3 ?~ float (-4))) $ outS $ boxS' id
    wallrender = render' (renderLight .~ (lights <$> steps)) [wallgeo] cam
  in
    do r <- newIORef mempty
       run r [outT $ compT 31 [compT 31 [lrender, lrender & blur (float 32)], wallrender]]

fade' f l o t = feedbackT t (\t' -> l $ compT 0 [t, levelT' (levelOpacity ?~ o) t']) f
fade = fade' id id

data Palette = Palette [Color]
data Color = Hex BS.ByteString | RGB Int Int Int
palette (Palette colors) = ramp' (topResolution .~ iv2 (128, 0)) . table
  . fromLists $ ["r", "g", "b", "a", "pos"]:(zipWith (colorToBS (length colors)) [0..] colors)

translate' f t = transformT' ((transformExtend ?~ int 3) . (transformTranslate .~ t) . f)
croppalette p s = crop' ((cropLeft ?~ s) . (cropRight ?~ s)) $ palette p

scr = (++) "scripts/"

neon = Palette $ Hex <$> ["A9336B", "5F2F88", "CB673D", "87BB38"]
fire = Palette $ Hex . BS.pack . fmap toUpper <$> ["f07f13", "800909", "f27d0c", "fdcf58"]
buddhist = Palette $ Hex . BS.pack . fmap toUpper <$> ["0000FF", "FFFF00", "FF0000", "FFFFFF", "FF9800"]
tealcontrast = Palette [RGB 188 242 246, RGB 50 107 113, RGB 211 90 30, RGB 209 122 43, RGB 188 242 246]
purplish = Palette [RGB 150 110 100, RGB 223 143 67, RGB 76 73 100 , RGB 146 118 133, RGB 165 148 180]
sunset = Palette [RGB 185 117 19, RGB 228 187 108, RGB 251 162 1, RGB 255 243 201]
coolpink = Palette [RGB 215 40 26, RGB 157 60 121, RGB 179 83 154, RGB 187 59 98]
darkestred = Palette [RGB 153 7 17, RGB 97 6 11, RGB 49 7 8, RGB 13 7 7, RGB 189 5 13]
nature = Palette [RGB 63 124 7, RGB 201 121 66, RGB 213 101 23, RGB 177 201 80, RGB 180 207 127]

colorToBS :: Int -> Int -> Color -> [BS.ByteString]
colorToBS n i (Hex str) =
  let
    hexes = chunksOf 2 . drop 1
    todig = flip L.elemIndex "0123456789ABCDEF"
    toIntList = fmap todig
    toInt = foldr (\i acc -> acc * 16 + i) 0
    toHex = fmap toInt . sequence . toIntList
    hextorgb = fmap (BS.pack . show . (/ 256) . fromIntegral)
  in
    catMaybes $ (hextorgb <$> (toHex <$> hexes (show str))) ++ [Just "1.0", Just . BS.pack . show $ fromIntegral i / fromIntegral n]
colorToBS n i (RGB r g b) =
  (++ [BS.pack . show $ fromIntegral i / fromIntegral n]) $ fmap (BS.pack . show . (/ 256) . fromIntegral) [r, g, b, 256]

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n = l `c` splitter (drop i l) c n
