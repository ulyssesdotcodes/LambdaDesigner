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

  'feedbackChop' : (feedbackCHOP, 'feedback', 'CHOP'),
  'noiseCHOP' : (noiseCHOP, 'noise', 'CHOP'),
  'sopToChop' : (soptoCHOP, 'sopto', 'CHOP'),

  'chopToSop' : (choptoSOP, 'chopto', 'SOP'),
  'circleSop' : (circleSOP, 'circle', 'SOP'),
  'noiseSop' : (noiseSOP, 'noise', 'SOP'),
  'outSop' : (outSOP, 'out', 'SOP'),
  'sphere' : (sphereSOP, 'sphere', 'SOP'),

  'constMat' : (constantMAT, 'constant', 'MAT'),

  'camera' : (cameraCOMP, 'cam', 'COMP'),
  'geo' : (geometryCOMP, 'geo', 'COMP'),
  'light' : (lightCOMP, 'light', 'COMP')
}

def receiveOSC(dat, rowIndex, message, bytes, timeStamp, address, args, peer):
  global classes

  addr = "/project1/lambda" + address
  if args[0] == "create":
    clazz = classes.get(args[1], 'none')
    if clazz == "none":
      print("Couldn't find " + args[1])
      return

    if op(addr) != None:
      if op(addr).type == clazz[1] and op(addr).family == clazz[2]:
        return
      else:
        op(addr).destroy()

    name = addr[(addr.rfind('/') + 1):]
    par = addr[:(addr.rfind('/'))]
    op(par).create(clazz[0], name)

    # TODO: Figure out a clean way to not special case these
    if clazz[1] == 'out' and clazz[2] == 'SOP':
      op(addr).render = True
      op(addr).display = True

    if clazz[1] == 'geo':
      op(addr + "/torus1").destroy()

  elif args[0] == "connect" and op(addr):
    op(addr).inputConnectors[args[1]].connect(op("/project1/lambda" + args[2]))

  elif args[0] == "parameter" and op(addr):
    pars = op(addr).pars(args[1])
    if len(pars) > 0:
      if isfloat(args[2]):
        pars[0].val = args[2]
      else:
        pars[0].expr = args[2]

  elif args[0] == "command" and op(addr):
    if args[1] == "pulse":
      pars = op(addr).pars(args[2])
      if len(pars) > 0:
        pars[0].pulse()

  return

def isfloat(value):
  try:
    float(value)
    return True
  except ValueError:
    return False
