#
# Demonstration of the package
#
#############################################################

# Get a connection to the database
con = getCon()

# Pull bWAR for 2012
war = getWAR(con)

#############################################################

# Examine the distribution of WAR
require(mosaic)
densityplot(~WAR, data=war)
favstats(~WAR, data=war)

# Show the leaders
head(war[order(war$WAR, decreasing=TRUE),], 30)

# Check your favorite team
nym = subset(war, teamId == 'NYM')
nym[order(nym$WAR, decreasing=TRUE),]

# Aggregate by team
byteam = aggregate(WAR ~ teamId, data=war, sum)
byteam[order(byteam$WAR, decreasing=TRUE),]

# Make a time series plot for a few players
player = getWAR(con, ids=c("wrighda03", "reyesjo01", "utleych01", "howarry01"), start=2003)
# Change lattice setting to get unified plot marks
sl = trellis.par.get("superpose.line")
sl$lwd = 3
trellis.par.set("superpose.line", sl)

ss = trellis.par.get("superpose.symbol")
ss$pch = 19
ss$cex = 2
ss$alpha = 0.7
trellis.par.set("superpose.symbol", ss)

xyplot(WAR ~ yearId, groups=playerId, data=player
       , type="b", main="WAR over Time"
       , xlab="Season", ylab="Wins Above Replacement (bWAR)"
       , par.settings = list("superpose.symbol" = ss, "superpose.line" = sl)
       , auto.key=TRUE
)

##############################################################

# Examine the reliability of WAR
rel = reliability(con, 1980)
xyplot(rel ~ as.numeric(names(rel)), type=c("r", "l"))
