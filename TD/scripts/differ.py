import scripts
from deepdiff import DeepDiff

state = {}

def apply(newState):
  # Step 1: create new nodes
  state = newState
  connections = []

  for key,value in state.items():
    addr = "/project1/lambda/" + key

    newOp = createOp(addr, value['ty'])

    if 'connections' in value:
      connections.extend(list(map(lambda c: c.append(addr), value['connections'])))

    if 'parameters' in value:
      for k,v in value['parameters']:
        addParameter(newOp, k, v)

    if 'commands' in value:
      for comm in value['commands']:
        runCommand(newOp, comm['command'], comm['args'])

    if 'text' in value:
      newOp.text = value['text']

  for key,conn in connections:
    op("/project1/lambda" + conn[0]).outputConnectors[0].connect(op(conn[1]).inputConnectors[key])

def createOp(addr, ty):
  clazz = scripts.getClass(ty, 'none')
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
  if len(pars) > 0:
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
    op(addr).par.reinitnet.pulse()

def runCommand(newOp, command, args):
    if command == "pulse":
      pars = op(addr).pars(args[0])
      if len(pars) > 0:
        if isfloat(args[1]):
          pars[0].pulse(float(args[1]), frames=float(args[2]))
        else:
          pars[0].pulse(args[1])
    elif command == "store":
      op(addr).store(args[0], args[1])

def isfloat(value):
  try:
    float(value)
    return True
  except ValueError:
    return False
