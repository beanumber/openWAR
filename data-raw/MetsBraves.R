library(openWAR)

# Get the data from a single game
MetsBraves = gameday("gid_2012_08_12_atlmlb_nynmlb_1")
save(MetsBraves, file = "data/MetsBraves.rda", compress = "xz")
