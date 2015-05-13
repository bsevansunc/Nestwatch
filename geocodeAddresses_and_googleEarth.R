###################################################################################################*
# Geocode addresses and display on Google Earth
###################################################################################################*

# 1. Determine the location of your current working directory (where your files are stored):

getwd()

# 2. IF you do not currently have lats and longs associated with your participant address file,
# run the following function to write a new file that contains this file. NOTE: the file MUST
# be in ".CSV" format!

# The inputs of the function is the file path to your address file and the file path to the new
# address file:

writeAddressToLatLong(addressFilePath = '/Users/bsevans/gits/Nestwatch/addressFileExample.csv',
                      outPath = '/Users/bsevans/gits/Nestwatch/addressFileExampleLL.csv')

# 3. Write KML. To do so, you must have already converted the participant address file to 
# an address file that includes LatLongs.

PointsToKML(addressFilePath = '/Users/bsevans/gits/Nestwatch/addressFileExampleLL.csv',
            outPath = '/Users/bsevans/gits/Nestwatch/siteExampleKML.kml')

# This file can now be viewed in either Google Earth or Google Maps

# To view in Google Earth, go to File/Open ... and select your file name to add the layer

# To view in Google Maps, follow the directions located here:
# https://support.google.com/mymaps/answer/3024836?hl=en
