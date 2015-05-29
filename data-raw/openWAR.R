library(openWAR)

year <- 2012
df.name <-paste("MLBAM", year, sep = "")
ow.name <- paste("openWAR", year, sep = ".")
ow.filename <- paste("data/", ow.name, ".rda", sep = "")

# assumes that MLBAMyear exists
raw.df <- eval(parse(text = df.name))

# It's rather large
print(object.size(raw.df), units = "Mb")

# Use the data from 2012 to compute WAR
# This may take a while...
madeWAR <- makeWAR(raw.df)
# Less than half the size of the raw data
print(object.size(madeWAR), units = "Mb")
# extract just the data frame
ow.plays <- madeWAR$openWAR
# save(openWARPlays.2012, file="data/openWARPlays.2012.rda")

# tabulate the WAR numbers
assign(ow.name, getWAR(ow.plays))
# why doesn't this work?
# save(get(ow.name), file = ow.filename)
temp <- get(ow.name)
save(temp, file = ow.filename)
