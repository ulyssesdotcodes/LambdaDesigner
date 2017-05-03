classes = {
  'chopToTop' : (choptoTOP, 'chopto', 'TOP'),
  'circleTop' : (circleTOP, 'circle', 'TOP'),
  'compositeTop' : (compositeTOP, 'comp', 'TOP'),
  'displace' : (displaceTOP, 'displace', 'TOP'),
  'feedbackTop' : (feedbackTOP, 'feedback', 'TOP'),
  'movieFileIn' : (moviefileinTOP, 'moviefilein', 'TOP'),
  'outTop' : (outTOP, 'out', 'TOP'),
  'nullTop' : (outTOP, 'null', 'TOP'),
  'render' : (renderTOP, 'render', 'TOP'),
  'levelTop' : (levelTOP, 'level', 'TOP'),
  'transform' : (transformTOP, 'transform', 'TOP'),
  'noiseTop' : (noiseTOP, 'noise', 'TOP'),
  'ramp' : (rampTOP, 'ramp', 'TOP'),
  'switchTop' : (switchTOP, 'switch', 'TOP'),
  'selectTop' : (selectTOP, 'select', 'TOP'),

  'constantChop' : (constantCHOP, 'constant', 'CHOP'),
  'count' : (countCHOP, 'count', 'CHOP'),
  'fan' : (fanCHOP, 'fan', 'CHOP'),
  'feedbackChop' : (feedbackCHOP, 'feedback', 'CHOP'),
  'hold' : (holdCHOP, 'hold', 'CHOP'),
  'logic' : (logicCHOP, 'logic', 'CHOP'),
  'math' : (mathCHOP, 'math', 'CHOP'),
  'mergeChop' : (mergeCHOP, 'merge', 'CHOP'),
  'noiseChop' : (noiseCHOP, 'noise', 'CHOP'),
  'sopToChop' : (soptoCHOP, 'sopto', 'CHOP'),
  'selectChop' : (selectCHOP, 'select', 'CHOP'),
  'timer' : (timerCHOP, 'timer', 'CHOP'),

  'chopToSop' : (choptoSOP, 'chopto', 'SOP'),
  'circleSop' : (circleSOP, 'circle', 'SOP'),
  'noiseSop' : (noiseSOP, 'noise', 'SOP'),
  'outSop' : (outSOP, 'out', 'SOP'),
  'sphere' : (sphereSOP, 'sphere', 'SOP'),

  'constMat' : (constantMAT, 'constant', 'MAT'),

  'chopExec' : (chopexecuteDAT, 'chopexecute', 'DAT'),
  'datExec' : (datexecuteDAT, 'datexecute', 'DAT'),
  'selectDat' : (selectDAT, 'select', 'DAT'),
  'table' : (tableDAT, 'table', 'DAT'),
  'textDat' : (textDAT, 'text', 'DAT'),
  'tcpip' : (tcpipDAT, 'tcpip', 'DAT'),

  'camera' : (cameraCOMP, 'cam', 'COMP'),
  'geo' : (geometryCOMP, 'geo', 'COMP'),
  'light' : (lightCOMP, 'light', 'COMP')
}

def getClass(opname, default):
  return classes.get(opname, default)
