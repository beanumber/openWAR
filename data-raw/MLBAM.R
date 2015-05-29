library(openWAR)

# Get the data for 2012
MLBAM2012 = getData(start = "2013-03-28", end = "2013-10-03")
save(MLBAM2012, file = "data/MLBAM2012.rda")

# Get the data for 2013
MLBAM2013 = getData(start = "2013-03-31", end = "2013-09-30")
save(MLBAM2013, file = "data/MLBAM2013.rda")