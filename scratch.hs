r <- topRunner

r $ triggerops (lowv)
  [ shapes (float 4) (float 0.1 !+ (float 3 !* highvc)) (float 0.2)
  , lines (float 1 !+ (float (-2) !* volc)) (float 0.2)
    & rotate (seconds !* float 180)
    & paletterepeatT neon (float 10)
  , sineT (float 1) (float 1) (float 1)
    & fade (float 0.4)
  ]
  & fade (float 0.9)


-- amoebas
r $ flocking (float 0.7, chopChan0 $ math' (opaddf $ 10) [lowv]) (volc !* float 9)
  & blur (float 20)
  & fade (float 0.9)
  & palettemap buddhist (sincycle 0.4 0.9)

r $ tdata (float 10) (chopToT $ bandPass (sincycle 0.7 0.6 !+ float 0.2) ain)
  & translatey (float (-0.2))
  & littleplanet
  & flowermod (seconds !* float 3)
  & scalexy (float 0.4)
  & palettecycle fire
  & val (float 3.5)
  & fade (float 0.96)


-- saved
-- amoebas
r $ flocking (float 0.3, lowvc !* float 0.01) (volc !* float 4)
  & blur (float 20)
  & fade (float 0.98)
  & palettemap buddhist (sincycle 0.4 0.9)
  & val (float 0.7)
  & sat (float 0.8)
