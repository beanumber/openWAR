#
# Build the data sets used in the package
#
#############################################################

require(openWAR)

# Get the data from a single game
MetsBraves = gameday()
save(MetsBraves, file = "data/MetsBraves.rda")

# Get the data for 2012
MLBAM2012 = getData(start = "2013-03-28", end = "2013-10-03")
save(MLBAM2012, file = "data/MLBAM2012.rda")

# Get the data for the first half of 2013
MLBAM2013 = getData(start = "2013-03-31", end = "2013-07-14")
save(MLBAM2013, file = "data/MLBAM2013.rda")

# Use the data from 2013 to compute WAR
openWAR.2013 = getWAR(MLBAM2013)
save(openWAR.2013, file="data/openWAR.2013.rda")

# Generated the uncertainty estimates
ds = makeWAR(MLBAM2013)
# Will take a loooooong time
openWAR.2013.sim = shakeWAR(ds, N = 2000)
save(openWAR.2013.sim, file="data/openWAR.2013.sim.rda")

# Build idmap
idmap = read.csv("data/id-map.csv", na.strings = c("NULL"))
save(idmap, file="data/idmap.rda")