  if dat.numRows > 0:
    base = op(me.fetch('base', '--')[1:])
    eff = op(dat[0, int(op(me.fetch('voteResult', '--')[1:])[0] + 1)])
    print(eff)
    effOp = parent().copy(eff)
    prev = base.inputs[0]
    prev.outputConnectors[0].connect(effOp)
    effOp.outputConnectors[0].connect(base)
