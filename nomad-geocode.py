import csv
from time import sleep
import pandas as pd
from geopy.geocoders import Nominatim
from geopy.exc import GeocoderTimedOut

# Read in Addresses
df = pd.read_csv('street_read.csv')

# Geocode addresses
geolocator = Nominatim()

with open('streets_geocoded.csv','w') as f1:
    writer=csv.writer(f1, delimiter=',',lineterminator='\n',)
    writer.writerow(['address', 'latitude', 'longitude'])
    for row in df['addy']:
    	try:
    		lat = geolocator.geocode(row, timeout = 60).latitude
    	except:
    		lat = 0
    	sleep(1)
    	try:
    		lon = geolocator.geocode(row, timeout = 60).longitude
    	except:
    		lon = 0
    	sleep(1)
        writer.writerow([row, lat, lon])