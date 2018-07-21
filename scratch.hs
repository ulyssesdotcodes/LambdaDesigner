{-# LANGUAGE OverloadedStrings #-}

import LambdaDesigner.Op
import LambdaDesigner.ParsedOps
import LambdaDesigner.Lib
import LambdaDesigner.JSONOutput

import Prelude hiding (floor, mod)

import Control.Lens
import Data.Char
import Data.IORef
import Data.Matrix
import Data.Maybe
import Data.List as L

import Debug.Trace

import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS

go = 
  let
    constctest = feedbackC (constantCHOP (constantCHOPvalue1 ?~ float 0.5) []) id (mathCHOP (mathCHOPpreoff ?~ float 4) . (:[])) & beatCHOP id . (:[])
  in do
    putStrLn . show $ printMessages $ compile ( [] :: [Tree CHOP] ) [(tox "toxes/Visuals/flockingGpu.tox" [] (nullCHOP id [])) :: Tree TOP] (mempty :: Messages)
    -- run r [ outT $ chopToT $ logic' (logicCombineChops ?~ int 1) [logic' (logicCombineChans ?~ int 6) [constC [float 5, floor (seconds !% float 10)]], constC [float 1]]]

-- go =
--   let
--     modlights = scriptD' (datVars .~ [ ("behaviours", Resolve $ behavioursT)
--                                      , ("lights", Resolve $ int numactors)
--                                      , ("attrs", Resolve $ int numattrs)
--                                      , ("set", Resolve $ fileD "scripts/setBehaviour.py")
--                                      , ("delta", Resolve $ fileD "scripts/deltaBehaviour.py")
--                                      , ("acc", Resolve $ fileD "scripts/accBehaviour.py")
--                                      , ("move", Resolve $ fileD "scripts/moveBehaviour.py")
--                                      , ("clamp", Resolve $ fileD "scripts/clampBehaviour.py")
--                                      , ("attract", Resolve $ fileD "scripts/attractBehaviour.py")
--                                      , ("copy", Resolve $ fileD "scripts/copyBehaviour.py")
--                                      , ("backandforth", Resolve $ fileD "scripts/backAndForthBehaviour.py")
--                                      , ("volume", Resolve $ analyze (int 6) ain)
--                                      , ("noise", Resolve $ noiseC' ((noiseCTranslate._3 ?~ seconds !* float 20) . (noiseCAmplitude ?~ float 0.001) . (noiseCChannels ?~ str "chan0 chan1 chan2 chan3")))
--                                      ]) "scripts/behavioursScriptDAT.py" []
--     edat = executeD' ((datVars .~ [ ("modlights", Resolve $ modlights)
--                                   ]
--                       ) . (executeDatFramestart ?~ "op(me.fetch(\"modlights\")[1:]).cook(force=True)")) []
--     -- step = 0.2
--     -- steps = [0, step .. 1]
--     -- band = chan0f . analyze (int 6) . flip bandPass ain
--     -- input = Input audioIn
--     -- ltranslate f = float (f * 1.2) !- float 0.6
--     -- lightgeo f ins = geo' ((geoTranslate._1 ?~ castf (cell (int f, int pX) ins))
--     --                        . (geoTranslate._2 ?~ castf (cell (int f, int pY) ins))
--     --                        . (geoTranslate._3 ?~ float 3)
--     --                        . (geoUniformScale ?~ float (step * 0.1))
--     --                        . (geoMat ?~ lmat (float $ fromIntegral f))
--     --                       ) (outS $ tubeS' (tubeHeight ?~ float 10))
--     -- lrender = render' id ((\f -> lightgeo f modlights) <$> [0..(numactors - 1)]) cam & hsvT' (hsvAdjValMult ?~ float 1.3)
--     -- palettecolor = croppalette buddhist
--     -- chopcol l n = palettecolor (float l !+ (seconds !* float 0.3)) & topToC & chanf n
--     -- lights l = light' ((lightColor .~ v3 (chopcol l 0) (chopcol l 1) (chopcol l 2))
--     --                    . (lightTranslate._1 ?~ ltranslate l)
--     --                    . (lightAttenuated ?~ bool True)
--     --                    . (lightAttenuationStart ?~ float 0)
--     --                    . (lightAttenuationEnd ?~ float 12)
--     --                    . (lightAttenuationRolloff ?~ float 2)
--     --                   )
--     -- lmat l = pbrM' (pbrEmitColorMap ?~ (palettecolor (l !+ (seconds !* float 0.3))))
--     -- wallgeo = geo' ((geoMat ?~ pbrM' ((pbrMetallic ?~ float 0) . (pbrBaseColorMap ?~ movieFileIn (str "C:/Users/ulyssesp/Downloads/concrete_bare_2159_2638_Small.jpg")))) . (geoUniformScale ?~ float 2) . (geoTranslate._3 ?~ float (3)) . (geoScale._2 ?~ float 0.5) . (geoScale._3 ?~ float 2)) $ outS $ boxS' id
--     -- wallrender = render' (renderLight .~ (lights <$> steps)) [wallgeo] cam
--     input = Input audioIn
--     ltranslate f = float (f * 1.2) !- float 0.6
--     lightgeo f ins = geo' ((geoTranslate .~ v3 (bchan f pX ins) (bchan f pY ins) (bchan f pZ ins))
--                            . (geoUniformScale ?~ float (0.02))
--                            . (geoMat ?~ lmat (bchan f col ins))
--                           ) (outS $ sphere)
--     lrender = render' id ((\f -> lightgeo f modlights) <$> [0..(numactors - 1)]) rendercam & hsvT' (hsvAdjValMult ?~ float 1.3)
--     palettecolor = croppalette tealcontrast
--     chopcol l n = palettecolor l & topToC & chanf n
--     lights ins l = light' ((lightColor .~ v3 (chopcol (bchan l col ins) 0) (chopcol (bchan l col ins) 1) (chopcol (bchan l col ins) 2))
--                           . (lightTranslate .~ v3 (bchan l pX ins) (bchan l pY ins) (bchan l pZ ins))
--                           . (lightAttenuated ?~ bool True)
--                           . (lightAttenuationStart ?~ float 0)
--                           . (lightAttenuationEnd ?~ float 3)
--                           . (lightAttenuationRolloff ?~ float 10)
--                           . (lightDimmer ?~ float 2)
--                           )
--     lmat l = pbrM' (pbrEmitColorMap ?~ (palettecolor l))
--     wallgeo = geo' ((geoMat ?~ pbrM' ((pbrMetallic ?~ float 0) . (pbrBaseColorMap ?~ movieFileIn (str "C:/Users/ulyssesp/Downloads/concrete_bare_2159_2638_Small.jpg")))) . (geoUniformScale ?~ float 2) . (geoScale._2 ?~ float 0.25) . (geoScale._3 ?~ float 1)) $ outS $ boxS' id
--     centerCam t r l = cam' ((camTranslate .~ t) . (camPivot .~ v3mult (float (-1)) t) . (camRotate .~ r) . (camLookAt ?~ l))
--     rendercam = centerCam (v3 (float 3) (float 2) (float 3)) emptyV3 wallgeo
--     wallrender = render' ((renderLight .~ (lights modlights <$> [0..(numactors - 1)])) . (renderCullFace ?~ int 2)) [wallgeo] rendercam
--   in
--     do r <- newIORef mempty
--        run r [ outT $ compT 31 [compT 31 [lrender, lrender & blur (float 32)], wallrender] ]

-- ain = math' (mathMult ?~ float 0.1) [audioIn]
-- volume = chan0f $ analyze (int 6) ain
-- op0 = opInput (int 0)
-- vX = 0
-- vY = 1
-- vZ = 2
-- pX = 3
-- pY = 4
-- pZ = 5
-- col = 6
-- numattrs = 7
-- initLight = constC $ replicate numattrs (float 0)
-- initLights = mergeC $ replicate numactors initLight
-- modinputs = V.fromList $ flip chanf op0 <$> [0..(numattrs * numactors)]

-- bchan :: Int -> Int -> Tree DAT -> Tree Float
-- bchan i ch = castf . cell (int i, int ch)


-- data Input = Input (Tree CHOP)
-- data Behaviour = Set Int Float String
--                | Delta Int Float Int String
--                | Add Int Float
--                | Clamp Int Int Float Float
--                | BackAndForth Int Int Float String
--                | Diminish Int Float
--                | Acc Int Int
--                | Attract Int Int Int Int Int Int  Float Float
--                | CopyNearest Int Int Int Int Float
--                | Conshaviour Behaviour Behaviour

-- posupdate = Conshaviour (Acc pX vX)
--             $ Conshaviour (Diminish vX 0.99)
--             $ Conshaviour (Acc pY vY)
--             $ Conshaviour (Diminish vY 0.99)
--             $ Conshaviour (Acc pZ vZ)
--             $ Diminish vZ 0.99

-- lightclamp = Conshaviour (Clamp pX vX (-0.5) 0.5) $
--              Conshaviour (Clamp pY vY (-0.2) 0.2) $
--              Clamp pZ vZ (-0.5) 0.5

-- behaviours = [ [Delta vY 0.6 0 "volume", Add vY (-0.008), Delta vX 0.3 0 "noise", Delta vZ 0.3 1 "noise", Delta col 0.4 0 "volume", lightclamp, posupdate]
--              , [BackAndForth pZ vZ 0.12 "volume", BackAndForth pX vX 3 "noise", lightclamp, posupdate]
--              , [Attract pX pY pZ vX vY vZ 1 0.001, CopyNearest pX pY pZ col 0.02, lightclamp, posupdate]
--              , [Attract pX pY pZ vX vY vZ 1 0.0001, CopyNearest pX pY pZ col 0.04, lightclamp, posupdate]
--              , [Attract pX pY pZ vX vY vZ 1 0.0001, Attract pX pY pZ vX vY vZ 0.2 (-0.001), Add col 0.001, lightclamp, posupdate]
--              ]

-- numactors = length behaviours

-- scriptCDAT :: String -> (DAT -> DAT) -> [Tree CHOP] -> Tree CHOP
-- scriptCDAT file df = N <$> ScriptCHOP (fileD' df file)

-- blist :: Int -> Behaviour -> [[BS.ByteString]]
-- blist i (Set c v d) = (:[]) $ BS.pack <$> ["set", show i, show c, show v, d]
-- blist i (Delta c v chan d) = (:[]) $ BS.pack <$> ["delta", show i, show c, show v, show chan, d]
-- blist i (Acc a b) = (:[]) $ BS.pack <$> ["acc", show i, show a, show b]
-- blist i (Add c v) = (:[]) $ BS.pack <$> ["move", show i, show c, show v]
-- blist i (Clamp clamper clamped mn mx) = (:[]) $ BS.pack <$> ["clamp", show i, show clamper, show clamped, show mn, show mx]
-- blist i (BackAndForth base change mult delta) = (:[]) $ BS.pack <$> ["backandforth", show i, show base, show change, show mult, delta]
-- blist i (Attract x y z vx vy vz r v) = (:[]) $ BS.pack <$> ["attract", show i, show x, show y, show z, show vx, show vy, show vz, show r, show v]
-- blist i (CopyNearest x y z t v) = (:[]) $ BS.pack <$> ["copy", show i, show x, show y, show z, show t, show v]
-- blist i (Conshaviour b b') = blist i b ++ blist i b'
-- blist _ _ = []
-- bslist i = fmap (blist i)


-- padlist :: [BS.ByteString] -> [BS.ByteString]
-- padlist l = l ++ replicate ((maximum $ length <$> behavioursM) - (length l)) "0"
-- behavioursM = mconcat . mconcat $ zipWith bslist [0..] behaviours
-- paddedBehavioursM = padlist <$> filter (not . null) behavioursM
-- behavioursT = table $ fromLists paddedBehavioursM

-- band :: Tree CHOP -> Tree Float -> Tree Float
-- band ain = chan0f . analyze (int 6) . flip bandPass ain

-- fade' f l o t = feedbackT t (\t' -> l $ compT 0 [t, levelT' (levelOpacity ?~ o) t']) f
-- fade = fade' id id

-- scr = (++) "scripts/"

-- neon = Palette $ Hex <$> ["A9336B", "5F2F88", "CB673D", "87BB38"]
-- fire = Palette $ Hex . BS.pack . fmap toUpper <$> ["f07f13", "800909", "f27d0c", "fdcf58"]
-- buddhist = Palette $ Hex . BS.pack . fmap toUpper <$> ["0000FF", "FFFF00", "FF0000", "FFFFFF", "FF9800"]
-- tealcontrast = Palette [RGB 188 242 246, RGB 50 107 113, RGB 211 90 30, RGB 209 122 43, RGB 188 242 246]
-- purplish = Palette [RGB 150 110 100, RGB 223 143 67, RGB 76 73 100 , RGB 146 118 133, RGB 165 148 180]
-- sunset = Palette [RGB 185 117 19, RGB 228 187 108, RGB 251 162 1, RGB 255 243 201]
-- coolpink = Palette [RGB 215 40 26, RGB 157 60 121, RGB 179 83 154, RGB 187 59 98]
-- darkestred = Palette [RGB 153 7 17, RGB 97 6 11, RGB 49 7 8, RGB 13 7 7, RGB 189 5 13]
-- nature = Palette [RGB 63 124 7, RGB 201 121 66, RGB 213 101 23, RGB 177 201 80, RGB 180 207 127]

-- colorToBS :: Int -> Int -> Color -> [BS.ByteString]
-- colorToBS n i (Hex str) =
--   let
--     hexes = chunksOf 2 . drop 1
--     todig = flip L.elemIndex "0123456789ABCDEF"
--     toIntList = fmap todig
--     toInt = foldr (\i acc -> acc * 16 + i) 0
--     toHex = fmap toInt . sequence . toIntList
--     hextorgb = fmap (BS.pack . show . (/ 256) . fromIntegral)
--   in
--     catMaybes $ (hextorgb <$> (toHex <$> hexes (show str))) ++ [Just "1.0", Just . BS.pack . show $ fromIntegral i / fromIntegral n]
-- colorToBS n i (RGB r g b) =
--   (++ [BS.pack . show $ fromIntegral i / fromIntegral n]) $ fmap (BS.pack . show . (/ 256) . fromIntegral) [r, g, b, 256]

-- build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
-- build g = g (:) []

-- chunksOf :: Int -> [e] -> [[e]]
-- chunksOf i ls = map (take i) (build (splitter ls)) where
--   splitter :: [e] -> ([e] -> a -> a) -> a -> a
--   splitter [] _ n = n
--   splitter l c n = l `c` splitter (drop i l) c n

-- data Palette = Palette [Color]
-- data Color = Hex BS.ByteString | RGB Int Int Int
-- palette (Palette colors) = ramp' (topResolution .~ iv2 (128, 0)) . table
--   . fromLists $ ["r", "g", "b", "a", "pos"]:(zipWith (colorToBS (length colors)) [0..] colors)

-- translate' f t = transformT' ((transformExtend ?~ int 3) . (transformTranslate .~ t) . f)
-- croppalette p s = crop' ((cropLeft ?~ s) . (cropRight ?~ s)) $ palette p



--   -- let
--   --   step = 0.2
--   --   steps = [0, step .. 1]
--   --   band = chan0f . analyze (int 6) . flip bandPass ain
--   --   input = Input audioIn
--   --   ltranslate f = float (f * 1.2) !- float 0.6
--   --   lightgeo f ins = geo' ((geoTranslate._1 ?~ chanf (f * numattrs + pX) ins)
--   --                          . (geoTranslate._2 ?~ chanf (f * numattrs + pY) ins)
--   --                          . (geoTranslate._3 ?~ float 3)
--   --                          . (geoUniformScale ?~ float (step * 0.1))
--   --                          . (geoMat ?~ lmat (float $ fromIntegral f))
--   --                         ) (outS $ tubeS' (tubeHeight ?~ float 10))
--   --   lrender = render' id ((\f -> lightgeo f modlights) <$> [0..(numactors - 1)]) cam & hsvT' (hsvAdjValMult ?~ float 1.3)
--   --   palettecolor = croppalette buddhist
--   --   chopcol l n = palettecolor (float l !+ (seconds !* float 0.3)) & topToC & chanf n
--   --   lights l = light' ((lightColor .~ v3 (chopcol l 0) (chopcol l 1) (chopcol l 2))
--   --                      . (lightTranslate._1 ?~ ltranslate l)
--   --                      . (lightAttenuated ?~ bool True)
--   --                      . (lightAttenuationStart ?~ float 0)
--   --                      . (lightAttenuationEnd ?~ float 12)
--   --                      . (lightAttenuationRolloff ?~ float 2)
--   --                     )
--   --   lmat l = pbrM' (pbrEmitColorMap ?~ (palettecolor (l !+ (seconds !* float 0.3))))
--   --   wallgeo = geo' ((geoMat ?~ pbrM' ((pbrMetallic ?~ float 0) . (pbrBaseColorMap ?~ movieFileIn (str "C:/Users/ulyssesp/Downloads/concrete_bare_2159_2638_Small.jpg")))) . (geoUniformScale ?~ float 2) . (geoTranslate._3 ?~ float (3)) . (geoScale._2 ?~ float 0.5) . (geoScale._3 ?~ float 2)) $ outS $ boxS' id
--   --   wallrender = render' (renderLight .~ (lights <$> steps)) [wallgeo] cam
--   -- in
--   --   do r <- newIORef mempty
--   --      run r [ outT $ compT 31 [compT 31 [lrender, lrender & blur (float 32)], wallrender] ]

-- -- data Input = Input (Tree CHOP)
-- -- data Behaviour = VolUp | GoDown | TopOut | StartX Float

-- -- behaviours = [[VolUp, GoDown, TopOut], [StartX 0.2]]
-- -- numactors = length behaviours

-- -- replaceexprs :: [(Int, Tree Float)] -> (Tree CHOP -> Tree CHOP)
-- -- replaceexprs rexp = expressionC (V.toList $ modinputs V.// rexp) . (:[])

-- -- modlights = fix "feedbacks" $ feedbackC (mergeC $ zipWith ($) (zipWith (\i bs -> foldr (.) id $ behaviourInit i <$> bs) [0..] behaviours) $ (\i -> selLight i initLights) <$> [0..(numactors-1)]) id id

-- --                 -- . expressionC (V.toList $ modinputs V.// (mconcat $
-- --                 --                (\i -> [ (i * numattrs + pX, chanf (i * numattrs + pX) op0 !+ chanf (i * numattrs + vX) op0 )
-- --                 --                       , (i * numattrs + pY, chanf (i * numattrs + pY) op0 !+ chanf (i * numattrs + vY) op0 )
-- --                 --                       , (i * numattrs + vX, chanf (i * numattrs + vX) op0 !* float 0.99)
-- --                 --                       , (i * numattrs + vY, chanf (i * numattrs + vY) op0 !* float 0.99)
-- --                 --                       ]) <$> [0..(numactors - 1)])
-- --                 --               ) . (:[]))

-- -- selLight i = deleteCNum' (deleteCNonScoped ?~ bool True) (str $ mconcat ["[", show (i * numattrs), "-", show ((i + 1) * numattrs - 1),"]"])

-- -- replacechans :: [(Int, Tree CHOP -> Tree CHOP)] -> (Tree CHOP -> Tree CHOP)
-- -- replacechans rexp = (\ins -> replaceC [ins, mergeC . (fmap ($ ins)) $ (\(i, e) -> e . selectCConnect' (selectCNames ?~ PyExpr (BS.concat $ ["me.inputs[0][", BS.pack $ show i, "].name"]))) <$> rexp])

-- -- bindex i ch = i * numattrs + ch
-- -- bchan i ch = chanf (bindex i ch) modlights

-- -- behaviourOp :: Int -> Behaviour -> (Tree CHOP -> Tree CHOP)
-- -- behaviourOp i VolUp = replacechans [ (bindex i vY, math' (mathAdd .~ Just (volume !* float 0.02)) . (:[])) ]
-- -- behaviourOp i GoDown = replacechans [ (bindex i vY, replaceexprs [(0, ternary (bchan i pY !> float (-0.2)) (bchan 0 0 !- float 0.008) (float 0))])]
-- -- behaviourOp i TopOut = replacechans [ (bindex i vY, replaceexprs [(0, ternary (bchan i pY !> float (-0.2)) (bchan 0 0 !- float 0.008) (float 0))])]
-- -- behaviourOp i _ = id

-- -- behaviourInit :: Int -> Behaviour -> (Tree CHOP -> Tree CHOP)
-- -- behaviourInit i (StartX x) = replaceexprs [ (2, float x) ]
-- -- behaviourInit i _ = id

-- -- ain = math' (mathMult ?~ float 1) [audioIn]
-- -- volume = chan0f $ analyze (int 6) ain

-- -- op0 = opInput (int 0)
-- -- vX = 0
-- -- vY = 1
-- -- pX = 2
-- -- pY = 3
-- -- numattrs = 4
-- -- initLight = constC $ replicate numattrs (float 0)
-- -- initLights = mergeC $ replicate numactors initLight
-- -- modinputs = V.fromList $ flip chanf op0 <$> [0..(numattrs * numactors)]

-- -- band :: Tree CHOP -> Tree Float -> Tree Float
-- -- band ain = chan0f . analyze (int 6) . flip bandPass ain

-- -- fade' f l o t = feedbackT t (\t' -> l $ compT 0 [t, levelT' (levelOpacity ?~ o) t']) f
-- -- fade = fade' id id

-- -- data Palette = Palette [Color]
-- -- data Color = Hex BS.ByteString | RGB Int Int Int
-- -- palette (Palette colors) = ramp' (topResolution .~ iv2 (128, 0)) . table
-- --   . fromLists $ ["r", "g", "b", "a", "pos"]:(zipWith (colorToBS (length colors)) [0..] colors)

-- -- translate' f t = transformT' ((transformExtend ?~ int 3) . (transformTranslate .~ t) . f)
-- -- croppalette p s = crop' ((cropLeft ?~ s) . (cropRight ?~ s)) $ palette p

-- -- scr = (++) "scripts/"

-- -- neon = Palette $ Hex <$> ["A9336B", "5F2F88", "CB673D", "87BB38"]
-- -- fire = Palette $ Hex . BS.pack . fmap toUpper <$> ["f07f13", "800909", "f27d0c", "fdcf58"]
-- -- buddhist = Palette $ Hex . BS.pack . fmap toUpper <$> ["0000FF", "FFFF00", "FF0000", "FFFFFF", "FF9800"]
-- -- tealcontrast = Palette [RGB 188 242 246, RGB 50 107 113, RGB 211 90 30, RGB 209 122 43, RGB 188 242 246]
-- -- purplish = Palette [RGB 150 110 100, RGB 223 143 67, RGB 76 73 100 , RGB 146 118 133, RGB 165 148 180]
-- -- sunset = Palette [RGB 185 117 19, RGB 228 187 108, RGB 251 162 1, RGB 255 243 201]
-- -- coolpink = Palette [RGB 215 40 26, RGB 157 60 121, RGB 179 83 154, RGB 187 59 98]
-- -- darkestred = Palette [RGB 153 7 17, RGB 97 6 11, RGB 49 7 8, RGB 13 7 7, RGB 189 5 13]
-- -- nature = Palette [RGB 63 124 7, RGB 201 121 66, RGB 213 101 23, RGB 177 201 80, RGB 180 207 127]

-- -- colorToBS :: Int -> Int -> Color -> [BS.ByteString]
-- -- colorToBS n i (Hex str) =
-- --   let
-- --     hexes = chunksOf 2 . drop 1
-- --     todig = flip L.elemIndex "0123456789ABCDEF"
-- --     toIntList = fmap todig
-- --     toInt = foldr (\i acc -> acc * 16 + i) 0
-- --     toHex = fmap toInt . sequence . toIntList
-- --     hextorgb = fmap (BS.pack . show . (/ 256) . fromIntegral)
-- --   in
-- --     catMaybes $ (hextorgb <$> (toHex <$> hexes (show str))) ++ [Just "1.0", Just . BS.pack . show $ fromIntegral i / fromIntegral n]
-- -- colorToBS n i (RGB r g b) =
-- --   (++ [BS.pack . show $ fromIntegral i / fromIntegral n]) $ fmap (BS.pack . show . (/ 256) . fromIntegral) [r, g, b, 256]

-- -- build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
-- -- build g = g (:) []

-- -- chunksOf :: Int -> [e] -> [[e]]
-- -- chunksOf i ls = map (take i) (build (splitter ls)) where
-- --   splitter :: [e] -> ([e] -> a -> a) -> a -> a
-- --   splitter [] _ n = n
-- --   splitter l c n = l `c` splitter (drop i l) c n
