# me - this DAT
# 
# dat - the DAT that received a message
# rowIndex - the row number the message was placed into
# message - an ascii representation of the data
#           Unprintable characters and unicode characters will
#           not be preserved. Use the 'bytes' parameter to get
#           the raw bytes that were sent.
# bytes - a byte array of the message.
# timeStamp - the arrival time component the OSC message
# address - the address component of the OSC message
# args - a list of values contained within the OSC message
# peer - a Peer object describing the originating message
#   peer.close()    #close the connection
#   peer.owner  #the operator to whom the peer belongs
#   peer.address    #network address associated with the peer
#   peer.port       #network port associated with the peer
#

import scripts
import json

def receiveOSC(dat, rowIndex, message, bytes, timeStamp, address, args, peer):
  if address == "/json":
    scripts.apply(json.loads(args[0]))
    return

  addr = "/project1/lambda" + address
  if args[0] == "create":
    clazz = scripts.getClass(args[1], 'none')
    if clazz == "none":
      print("Couldn't find " + args[1])
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
      return

    op(par).create(clazz[0], name)

    # TODO: Figure out a clean way to not special case these
    if clazz[1] == 'out' and clazz[2] == 'SOP':
      op(addr).render = True
      op(addr).display = True

    if clazz[1] == 'geo':
      op(addr + "/torus1").destroy()

  elif args[0] == "connect" and op(addr):
    op("/project1/lambda" + args[2]).outputConnectors[0].connect(op(addr).inputConnectors[args[1]])

  elif (args[0] == "parameter" or args[0] == "custompar") and op(addr):
    pars = op(addr).pars(args[1])
    if len(pars) > 0:
      if isfloat(args[2]):
        if pars[0].isMenu:
          pars[0].menuIndex = args[2]
        else:
          pars[0].val = float(args[2])
      else:
        pars[0].expr = args[2]
    if args[1] == "externaltox":
      op(addr).par.reinitnet.pulse()

  elif args[0] == "command" and op(addr):
    if args[1] == "pulse":
      pars = op(addr).pars(args[2])
      if len(pars) > 0:
        if isfloat(args[3]):
          pars[0].pulse(float(args[3]), frames=float(args[4]))
        else:
          pars[0].pulse(args[3])
    elif args[1] == "store":
      op(addr).store(args[2], args[3])

  elif args[0] == "text" and op(addr):
    op(addr).text = args[1]

  return

def isfloat(value):
  try:
    float(value)
    return True
  except ValueError:
    return False
