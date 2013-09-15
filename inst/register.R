##
## Download the Baseball Register from Ted Turocy's site

url = "http://www.chadwick-bureau.com/data/register/register-20130331.zip"
download.file(url, destfile = "data/register.zip")

# Unzip it
system("unzip \"data/register.zip\" -d \"data/\"")
# move the file back to the data directory
system("mv \"data/register-20130331/register.csv\" data/")
# remove the empty directory
# system("rm -R data/register-20130331")

register = read.csv("data/register.csv")

idmap = subset(register, !is.na(key_mlbam), select=c("key_person", "key_mlbam", "key_retro", "key_bbref", "key_bbpro"
                                                    , "key_fangraphs", "name_last", "name_first", "name_given"))

subset(idmap, name_last == "Trout")
# save it to the data folder
save(idmap, file="data/idmap.rda")
