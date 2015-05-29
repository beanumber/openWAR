##
## Download the Baseball Register from Ted Turocy's site
## http://chadwick-bureau.com/the-register/

library(dplyr)

url = "http://www.chadwick-bureau.com/data/register/register-20150405.zip"
download.file(url, destfile = "inst/extdata/register.zip")

# Unzip it
system("unzip \"inst/extdata/register.zip\" -d \"inst/extdata/\"")
# move the file back to the data directory
system("mv \"inst/extdata/register-20150405/register.csv\" inst/extdata/")
# remove the empty directory
system("rm -R inst/extdata/register-20150405")
# remove the ZIP file
system("rm inst/extdata/register.zip")


register = read.csv(system.file("extdata", "register.csv", package = "openWAR"))

idTT <- register %>%
  filter(!is.na(key_mlbam)) %>%
  select(key_person, key_mlbam, key_retro, key_bbref, key_bbpro
           , key_fangraphs, name_last, name_first, name_given)

# Examine Mike Trout
# filter(idTT, name_last == "Trout")
# save it to the data folder
save(idTT, file="data/idTT.rda")
