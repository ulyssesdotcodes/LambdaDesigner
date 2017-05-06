def onStart(timerOp):
	return

def onTimerPulse(timerOp, segment):
	return

def whileTimerActive(timerOp, segment, cycle, fraction):
	return

def onSegmentEnter(timerOp, segment, interrupt):
  return

def onSegmentExit(timerOp, segment, interrupt):
  timerOp.par.play = 0
  mod('server').enableVotes(0)
  return

def onCycleStart(timerOp, segment, cycle):
	return

def onCycleEndAlert(timerOp, segment, cycle, alertSegment, alertDone, interrupt):
	return

def onCycle(timerOp, segment, cycle):
	return

def onDone(timerOp, segment, interrupt):
	return

