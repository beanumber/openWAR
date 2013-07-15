# Python 2 code
import urllib
import zipfile
import os

batting = 'http://www.baseball-reference.com/data/war_daily_bat.txt'
pitching = 'http://www.baseball-reference.com/data/war_daily_pitch.txt'

root = "../"
data_dir = root + "data/"
 
# Download the BB-Ref WAR and save it in the data directory 
print "downloading BB-Ref WAR..."
# urllib.urlretrieve(batting, data_dir + "war_bbref_bat.txt")
# urllib.urlretrieve(pitching, data_dir + "war_bbref_pitch.txt")

# Import it into MySQL

sqlinit = root + "sql/war_bbref.sql"
sqlcmd = "mysql -hmacgarnagle.no-ip.org -uopenwar -pX9SaZz7jztb3JtVE openwar" 

os.system(sqlcmd + "< \"" + sqlinit + "\"")

# Build the LOAD DATA commands
sqlimport1 = "\nLOAD DATA LOCAL INFILE \"" + data_dir + "war_bbref_bat.txt\" INTO TABLE war_bbref_batting FIELDS TERMINATED BY \",\" IGNORE 1 lines;"
sqlimport2 = "\nLOAD DATA LOCAL INFILE \"" + data_dir + "war_bbref_pitch.txt\" INTO TABLE war_bbref_pitching FIELDS TERMINATED BY \",\" IGNORE 1 lines;"

# Create a SQL script to import the data
# This is clunky but it will work for now
sql_file = open(root + "sql/war_bbref_import.sql", "w")
sql_file.write(sqlimport1)
sql_file.write(sqlimport2)
sql_file.close()

print "importing the BB-Ref WAR into MySQL..."
os.system(sqlcmd + " < \"" + root + "sql/war_bbref_import.sql\"")

