library(openWAR)

May <- getData(start = "2013-05-01", end = "2013-05-31")
# It takes up a decent amount of memory!
print(object.size(May), units = "Mb")
save(May, file = "data/May.rda", compress = "xz")



MayProcessed <- makeWAR(May)
save(MayProcessed, file = "data/MayProcessed.rda", compress = "xz")
