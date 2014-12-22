#
# Build the data sets used in the package
#
#############################################################

require(openWAR)

# Get the data from a single game
MetsBraves = gameday()
save(MetsBraves, file = "data/MetsBraves.rdata")

# Get the data for 2012
MLBAM2012 = getData(start = "2013-03-28", end = "2013-10-03")
save(MLBAM2012, file = "data/MLBAM2012.rda")

# Get the data for 2013
MLBAM2013 = getData(start = "2013-03-31", end = "2013-09-30")
save(MLBAM2013, file = "data/MLBAM2013.rda")

# Use the data from 2012 to compute WAR
madeWAR2012 = makeWAR(MLBAM2012)
openWARPlays.2012 = madeWAR2012$openWAR
save(openWARPlays.2012, file="data/openWARPlays.2012.rda")

openWAR.2012 = getWAR(openWARPlays.2012)
save(openWAR.2012, file="data/openWAR.2012.rda")

# Use the data from 2013 to compute WAR
openWAR.2013 = getWAR(madeWAR2013$openWAR)
save(openWAR.2013, file="data/openWAR.2013.rda")

# Generated the uncertainty estimates
ds = makeWAR(MLBAM2013)
# Will take a loooooong time
openWAR.2013.sim = shakeWAR(ds, N = 2000)
save(openWAR.2013.sim, file="data/openWAR.2013.sim.rda")

# Build idmap
idmap = read.csv("data/id-map.csv", na.strings = c("NULL"))
save(idmap, file="data/idmap.rda")

# Cross-check our WAR with bbref
rWAR = getrWAR()
rWAR2012 = subset(rWAR, yearId == 2012)

ids = getIDMap()

data(idmap)
WAR = merge(x=rWAR2012, y=ids, by.x="playerId", by.y="playerID")
WAR = merge(x=WAR, y=players, by.x="mlbamId", by.y="batterId")

panel.compare = function (x, y,...) {
  panel.xyplot(x,y, pch=19, alpha=0.3, ...)
  panel.abline(0,1, col="darkgray")
  panel.text(0, 0, paste("Correlation =", round(cor(x,y, use="complete.obs"), 3)))
}

xyplot(RAA_bat ~ RAA.bat, data=WAR, type=c("p", "r", "smooth")
       , panel=panel.compare
       , main="Comparison of openWAR vs. rWAR, 2012"
       , xlab = "Batting Runs Above Average (openWAR)", ylab="Batting Runs Above Average (rWAR)")

with(WAR, cor(RAA_bat, RAA.bat))

favstats(~RAA.bat, data=players)
favstats(~RAA_bat, data=rWAR2012)
densityplot(~RAA_bat, data=rWAR2012)
ladd(panel.densityplot(players$RAA.bat, col="pink"))

