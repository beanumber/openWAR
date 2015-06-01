library(openWAR)
library(openWARData)

openWAR2012 <- getWAR(openWARPlays2012)
save(openWAR2012, file = "data/openWAR2012.rda")

openWAR2013 <- getWAR(openWARPlays2013)
save(openWAR2013, file = "data/openWAR2013.rda")

openWAR2014 <- getWAR(openWARPlays2014)
save(openWAR2014, file = "data/openWAR2014.rda")
