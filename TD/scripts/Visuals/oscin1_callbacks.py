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

toEffectsDict = {}
fromEffectsDict = {}

toInputsDict = {}

def receiveOSC(dat, rowIndex, message, bytes, timeStamp, address, args, peer):
  if(len(args) > 0):
    global toEffectsDict
    global fromEffectsDict
    global toInputsDict

    opName = args[0]
    uniforms = op('uniforms_' + opName)
    curOp = op(opName)

    if(address == "/progs"):
      progType = args[1]

      if(curOp != None and curOp.par.Program != progType):
        curOp.destroy()
        curOp = None
        toInputsDict[opName] = []

      newProg = op(progType)
      if(newProg != None and curOp == None):
        curOp = op('/project1').copy(op(newProg))
        curOp.name = opName

      if (opName in fromEffectsDict):
        fromOp = op(fromEffectsDict[opName])
        if fromOp != None and (not fromOp.outputs or fromOp.outputs[0] != curOp):
          if(len(fromOp.outputConnectors[0].connections) > 0):
            curOp.outputConnectors[0].connect(fromOp.outputs[0].inputConnectors[0])
          fromOp.outputConnectors[0].connect(curOp.inputConnectors[0])

      if(opName in toEffectsDict):
        toOp = op(toEffectsDict[opName])
        if toOp != None and (not curOp.outputs or curOp.outputs[0] != toOp):
          curOp.outputConnectors[0].connect(toOp.inputConnectors[0])

      baseName = prog_name(opName)

      if len(curOp.pars('Base')) > 0:
        curOp.par.Base = "../" + baseName + "_base"

      if opName[len(baseName):] == '0':
        baseOp = op(baseName + "_base")

        if baseOp == None:
          baseOp = op('/project1').create(nullTOP)
          baseOp.name = baseName + "_base"

        related = ops(baseName + "[1-99]")

        if len(related) == 0:
          curOp.outputConnectors[0].connect(baseOp.inputConnectors[0])
        else:
          curOp.outputConnectors[0].connect(related[0].inputConnectors[0])

          for idx, prog in enumerate(related[:-1]):
            related[idx].outputConnectors[0].connect(related[idx + 1].inputConnectors[0])

          related[-1].outputConnectors[0].connect(baseOp.inputConnectors[0])

        if(opName == "s0"):
          baseOp.outputConnectors[0].connect(op('sout'))

        if(opName == "z0"):
          baseOp.outputConnectors[0].connect(op('zout'))


    elif (address == "/progs/effect"):
      toEffectsDict[opName] = args[1]
      fromEffectsDict[args[1]] = opName
      effOp = op(args[1])
      if(effOp != None):
        if(len(curOp.outputs) > 0):
          effOp.outputConnectors[0].connect(curOp.outputs[0].inputConnectors[0])
        curOp.outputConnectors[0].connect(effOp.inputConnectors[0])

    elif (address == "/progs/effect/clear"):
      if (opName in toEffectsDict):
        delete_effect(toEffectsDict[opName])


    elif (address == "/progs/uniform") and op(opName) != None:
      uName = args[1]
      val = 0
      if args[2] == "input":
        inputs = toInputsDict[opName] if opName in toInputsDict else []
        newOpName = opName + '_input_' + uName

        newOp = op(newOpName)

        if newOp != None:
          if newOp.par.Type != args[3]:
            op(newOpName).destroy()
            newOp = op('/project1').copy(op(args[3]))
            inputs.append(newOp.name)
        else:
            newOp = op('/project1').copy(op(args[3]))
            inputs.append(newOp.name)

        newOp.name = newOpName
        toInputsDict[opName] = inputs
        val = "op('" + newOp.name + "').op('out1')"
        if type(newOp.op('out1')) == outCHOP:
          val += "[0]"
        if len(args) > 4:
          newOp.par.In1 = args[4]
        if len(args) > 5:
          newOp.par.In2 = args[5]
        if len(args) > 6:
          newOp.par.In3 = args[6]
        if len(args) > 6 and len(newOp.pars('In4')) > 0:
          newOp.par.In4 = args[7]
      elif args[2] == "string":
        val = "\"" + args[3].strip() + "\""
      else:
        val = args[2]

      parObj = op(opName).pars(uName.replace("_", "").capitalize())
      if len(parObj) > 0:
        if isinstance(val, str):
          parObj[0].expr = val
        else:
          parObj[0].val = val

    elif (address == "/progs/clear"):
      delete_effect(opName)

  return

def delete_effect(opName):
  if (opName in toEffectsDict):
    effName = toEffectsDict[opName]
    delete_effect(effName)
    del toEffectsDict[opName]

  curOp = op(opName)
  if(curOp != None):
    if len(curOp.inputs) > 0:
      curOp.inputs[0].outputConnectors[0].connect(curOp.outputs[0].inputConnectors[0])
    curOp.destroy()

  # if(op('uniforms_' + opName) != None):
  #   op('uniforms_' + opName).destroy()

def clear_dicts():
  toEffectsDict = {}
  fromEffectsDict = {}
  toInputsDict = {}

def prog_name(prog):
  return prog.translate({ord(k): None for k in '0123456789'})
