# me - this DAT
# scriptOp - the OP which is cooking
#
# press 'Setup Parameters' in the OP to call this function to re-create the parameters.
prevIndexVal = 0
count = 0
passthroughs = ""

def setupParameters(scriptOp):
  scriptOp.clear()
  page = scriptOp.appendCustomPage('Custom')
  p = page.appendStr('Passthroughs')
  p = page.appendFloat('Index')
  scriptOp.appendRow([ "path", "parameter", "value" ])
  scriptOp.appendRow([ "switch", "index", 0 ])
  return

# called whenever custom pulse parameter is pushed
def onPulse(par):
	return

def cook(scriptOp):
  global prevIndexVal
  global passthroughs
  global count

  if scriptOp[1,2] == None:
    setupParameters(scriptOp)

  ptsParam = scriptOp.par.Passthroughs.eval()
  
  pts = ptsParam.split()
  count = len(pts)

  indexParam = scriptOp.par.Index.eval()

  if indexParam != prevIndexVal and count > 0:
    scriptOp[1,2] = (indexParam * (count - 1) % count)
    prevIndexVal = indexParam

  return
