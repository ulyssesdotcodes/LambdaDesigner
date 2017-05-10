# me - this DAT
# scriptOp - the OP which is cooking
#
# press 'Setup Parameters' in the OP to call this function to re-create the parameters.
import struct

def setupParameters(scriptOp):
  page = scriptOp.appendCustomPage('Custom')
  p = page.appendPulse('Change')
  return

# called whenever custom pulse parameter is pushed
def onPulse(par):
  return

def cook(scriptOp):
  scriptOp.clear()
  scriptOp.appendRow(['pos', 'r', 'g', 'b', 'a'])
  inDat = scriptOp.inputs[0]

  colorCount = len(inDat.rows()) - 1
  for idx, r in enumerate(inDat.rows()):
    cs = list(map(lambda x: x / 256, struct.unpack('BBB',bytes.fromhex(r[0].val))))
    cs.insert(0, idx / colorCount)
    cs.append(1)
    scriptOp.appendRow(cs)
  return
