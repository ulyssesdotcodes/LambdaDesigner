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
