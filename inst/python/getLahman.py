# Python 2 code
import urllib
import zipfile
import os

url = 'http://seanlahman.com/files/database/lahman591-sql.zip'
root = "../"
data_dir = root + "data/"
zipname = "lahman591-sql.zip"
zippath = data_dir + zipname
 
# Download the Lahman database and save it in the data directory 
print "downloading the Lahman database..."
# urllib.urlretrieve(url, zippath)

# Unzip the Lahman DB
zip = zipfile.ZipFile(zippath)
zip.extractall(path = data_dir)

# Import it into MySQL
cmd = "mysql -hmacgarnagle.no-ip.org -uopenwar -pX9SaZz7jztb3JtVE openwar < \"" + data_dir + "lahman591.sql\"" + " > \"" + root + "log/lahman.log\""
print cmd

os.system(cmd)

