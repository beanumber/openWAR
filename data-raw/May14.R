library(openWAR)

# Get the data from a single game
May14 <- getData(start = "2013-05-14")
save(May14, file = "data/May14.rda", compress = "xz")
