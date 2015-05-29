library(openWAR)

data(MLBAM2013)
# Generated the uncertainty estimates
madeWAR = makeWAR(MLBAM2013)
# Will take a loooooong time
openWAR.2013.sim = shakeWAR(madeWAR, N = 10)
save(openWAR.2013.sim, file="data/openWAR.2013.sim.rda")
