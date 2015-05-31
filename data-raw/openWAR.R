library(openWAR)
library(openWARData)

openWAR.2012 <- getWAR(openWARPlays.2012)
save(openWAR.2012, file = "data/openWAR.2012.rda")

openWAR.2013 <- getWAR(openWARPlays.2013)
save(openWAR.2013, file = "data/openWAR.2013.rda")

openWAR.2014 <- getWAR(openWARPlays.2014)
save(openWAR.2014, file = "data/openWAR.2014.rda")
