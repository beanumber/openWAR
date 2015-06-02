library(openWAR)
library(openWARData)

openWAR2012 <- getWAR(openWARPlays2012)
save(openWAR2012, file = "data/openWAR2012.rda", compress = "xz")

openWAR2013 <- getWAR(openWARPlays2013)
save(openWAR2013, file = "data/openWAR2013.rda", compress = "xz")

openWAR2014 <- getWAR(openWARPlays2014)
save(openWAR2014, file = "data/openWAR2014.rda", compress = "xz")

openWAR2015 <- getWAR(openWARPlays2015)
save(openWAR2015, file = "data/openWAR2015.rda", compress = "xz")

