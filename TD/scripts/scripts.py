from dictdiffer import diff, dot_lookup

classes = {
  'blur' : (blurTOP, 'blur', 'TOP'),
  'chopToTop' : (choptoTOP, 'chopto', 'TOP'),
  'circleTop' : (circleTOP, 'circle', 'TOP'),
  'compositeTop' : (compositeTOP, 'comp', 'TOP'),
  'crop' : (cropTOP, 'crop', 'TOP'),
  'displace' : (displaceTOP, 'displace', 'TOP'),
  'edges' : (edgeTOP, 'edge', 'TOP'),
  'feedbackTop' : (feedbackTOP, 'feedback', 'TOP'),
  'flip' : (flipTOP, 'flip', 'TOP'),
  'glslTop' : (glslmultiTOP, 'glslMulti', 'TOP'),
  'movieFileIn' : (moviefileinTOP, 'moviefilein', 'TOP'),
  'inTop' : (inTOP, 'in', 'TOP'),
  'nullTop' : (nullTOP, 'null', 'TOP'),
  'outTop' : (outTOP, 'out', 'TOP'),
  'render' : (renderTOP, 'render', 'TOP'),
  'hsvAdjustTop' : (hsvadjustTOP, 'hsvadj', 'TOP'),
  'levelTop' : (levelTOP, 'level', 'TOP'),
  'transform' : (transformTOP, 'transform', 'TOP'),
  'noiseTop' : (noiseTOP, 'noise', 'TOP'),
  'ramp' : (rampTOP, 'ramp', 'TOP'),
  'switchTop' : (switchTOP, 'switch', 'TOP'),
  'selectTop' : (selectTOP, 'select', 'TOP'),
  'textTop' : (textTOP, 'text', 'TOP'),
  'videoDeviceIn' : (videodeviceinTOP, 'videodevin', 'TOP'),

  'analyze' : (analyzeCHOP, 'analyze', 'CHOP'),
  'audioIn' : (audiodeviceinCHOP, 'audiodevin', 'CHOP'),
  'audioFilter' : (audiofilterCHOP, 'audiofilter', 'CHOP'),
  'audioSpectrum' : (audiospectrumCHOP, 'audiospect', 'CHOP'),
  'constantChop' : (constantCHOP, 'constant', 'CHOP'),
  'count' : (countCHOP, 'count', 'CHOP'),
  'delay' : (delayCHOP, 'delay', 'CHOP'),
  'fan' : (fanCHOP, 'fan', 'CHOP'),
  'feedbackChop' : (feedbackCHOP, 'feedback', 'CHOP'),
  'hold' : (holdCHOP, 'hold', 'CHOP'),
  'lag' : (lagCHOP, 'lag', 'CHOP'),
  'logic' : (logicCHOP, 'logic', 'CHOP'),
  'inChop' : (inCHOP, 'in', 'CHOP'),
  'math' : (mathCHOP, 'math', 'CHOP'),
  'mergeChop' : (mergeCHOP, 'merge', 'CHOP'),
  'midiIn' : (midiinmapCHOP, 'midiinmap', 'CHOP'),
  'oscInChop' : (oscinCHOP, 'oscin', 'CHOP'),
  'outChop' : (outCHOP, 'out', 'CHOP'),
  'noiseChop' : (noiseCHOP, 'noise', 'CHOP'),
  'sopToChop' : (soptoCHOP, 'sopto', 'CHOP'),
  'selectChop' : (selectCHOP, 'select', 'CHOP'),
  'switchChop' : (switchCHOP, 'switch', 'CHOP'),
  'timer' : (timerCHOP, 'timer', 'CHOP'),

  'chopToSop' : (choptoSOP, 'chopto', 'SOP'),
  'circleSop' : (circleSOP, 'circle', 'SOP'),
  'lineSop' : (lineSOP, 'line', 'SOP'),
  'mergeSop' : (mergeSOP, 'merge', 'SOP'),
  'metaball' : (metaballSOP, 'metaball', 'SOP'),
  'noiseSop' : (noiseSOP, 'noise', 'SOP'),
  'inSop' : (inSOP, 'in', 'SOP'),
  'outSop' : (outSOP, 'out', 'SOP'),
  'sphere' : (sphereSOP, 'sphere', 'SOP'),
  'sweep' : (sweepSOP, 'sweep', 'SOP'),
  'transformSop' : (transformSOP, 'transform', 'SOP'),

  'inMat' : (inMAT, 'in', 'MAT'),
  'outMat' : (outMAT, 'out', 'MAT'),
  'constMat' : (constantMAT, 'constant', 'MAT'),

  'chopExec' : (chopexecuteDAT, 'chopexecute', 'DAT'),
  'datExec' : (datexecuteDAT, 'datexecute', 'DAT'),
  'inDat' : (inDAT, 'in', 'DAT'),
  'outDat' : (outDAT, 'out', 'DAT'),
  'scriptDat' : (scriptDAT, 'script', 'DAT'),
  'selectDat' : (selectDAT, 'select', 'DAT'),
  'table' : (tableDAT, 'table', 'DAT'),
  'textDat' : (textDAT, 'text', 'DAT'),
  'tcpip' : (tcpipDAT, 'tcpip', 'DAT'),

  'camera' : (cameraCOMP, 'cam', 'COMP'),
  'geo' : (geometryCOMP, 'geo', 'COMP'),
  'light' : (lightCOMP, 'light', 'COMP'),
  'base' : (baseCOMP, 'base', 'COMP')
}

def getClass(opname, default):
  return classes.get(opname, default)

state = {}
diffs = []

def apply(newState):
  global state
  print(newState)
  # Step 1: create new nodes
  prevState = state
  state = newState
  ddiff = diff(prevState, state)
  diffs.append(ddiff)

  for diffi in list(reversed(list(ddiff))):
    splits = diffi[1].split('.') if isinstance(diffi[1], str) else diffi[1]
    if diffi[1] == '':
      if diffi[0] == 'add':
        addAll(diffi[2])
      elif diffi[0] == 'remove':
        for key,value in diffi[2]:
          curop = op("/project1/lambda" + key)
          if curop != None:
            curop.destroy()
    elif splits[1] == 'connections':
      concatname = [str(x) for x in diffi[1] if isinstance(x, str)]
      diffip = diffi[1] if isinstance(diffi[1], str) or diffi[1] == '' else ".".join(concatname)
      item = dot_lookup(state, diffip, parent=True)
      curop = op(getName(splits[0]))
      for connector in curop.inputConnectors:
        connector.disconnect()
      for i, conn in enumerate(item['connections']):
        op(getName(conn)).outputConnectors[0].connect(curop.inputConnectors[i])
    elif splits[1] == 'parameters':
      curop = op(getName(splits[0]))
      if diffi[0] == 'add':
        for k,v in diffi[2]:
          addParameter(curop, k, v)
      elif diffi[0] == 'change':
        addParameter(curop, splits[2], diffi[2][1])
      elif diffi[0] == 'remove':
        print(diffi)
        print(splits)
        for param in diffi[2]:
          par = curop.pars(param[0])[0]
          if par.val:
            par.val = par.default

    elif splits[1] == 'text':
      op(getName(splits[0])).text = diffi[2][1]

def getName(name):
  return "/project1/lambda" + name

def addAll(state):
  connections = []
  for key,value in state:
    addr = getName(key)

    newOp = createOp(addr, value['ty'])

    if 'parameters' in value:
      pars = value['parameters'].items()
      tox = next((x for x in pars if x[0] == 'externaltox'), None)
      if tox != None:
        pars = [x for x in pars if x[0] != 'externaltox']
        pars.insert(0, tox)

      for k,v in pars:
        addParameter(newOp, k, v)

    if 'commands' in value:
      coms = value['commands']
      for comm in coms:
        runCommand(newOp, comm['command'], comm['args'])

    if 'text' in value and value['text'] != None:
      newOp.text = value['text']

    if 'connections' in value:
      connections.extend([c, addr, i] for i,c in enumerate(value['connections']))

  for conn in connections:
    if conn[0] == '' or op(getName(conn[0])) == None:
      continue
    if conn[2] == 0:
      for connector in op(conn[1]).inputConnectors:
        connector.disconnect()
    op(getName(conn[0])).outputConnectors[0].connect(op(conn[1]).inputConnectors[conn[2]])

def createOp(addr, ty):
  clazz = getClass(ty, 'none')
  if clazz == "none":
    print("Couldn't find " + ty)
    return

  name = addr[(addr.rfind('/') + 1):]
  par = addr[:(addr.rfind('/'))]

  if op(addr) != None:
    op(addr).destroy()

  # Special case things that can't have duplicates
  if clazz[1] == 'audiodevin' or clazz[1] == 'videodevin':
    if op(clazz[1]) == None:
      parent().create(clazz[0], clazz[1])
    if clazz[2] == "CHOP":
      selOp = selectCHOP
      selPar = 'chop'
    elif clazz[2] == "TOP":
      selOp = selectTOP
      selPar = 'top'

    op(par).create(selOp, name)
    op(addr).pars(selPar)[0].val = '/project1/' + clazz[1]
  else:
    op(par).create(clazz[0], name)

  newOp = op(addr)

  # TODO: Figure out a clean way to not special case these
  if clazz[1] == 'out' and clazz[2] == 'SOP':
    newOp.render = True
    newOp.display = True

  if clazz[1] == 'geo':
    op(addr + "/torus1").destroy()

  return newOp

def addParameter(newOp, name, value):
  pars = newOp.pars(name)
  par = pars[0]
  if isfloat(value):
    if par.isMenu:
      par.menuIndex = value
    else:
      par.val = float(value)
  else:
    par.expr = value

  # Special case loading tox as soon as we know source
  if name == "externaltox":
    newOp.par.reinitnet.pulse()
  elif name == 'file' and newOp.type == "text":
    newOp.par.loadonstartpulse.pulse()

def runCommand(newOp, command, args):
    if command == "pulse":
      pars = newOp.pars(args[0])
      if len(pars) > 0:
        if isfloat(args[1]):
          pars[0].pulse(float(args[1]), frames=float(args[2]))
        else:
          pars[0].pulse(args[1])
    elif command == "store":
      newOp.store(args[0], args[1])

def isfloat(value):
  try:
    float(value)
    return True
  except ValueError:
    return False
