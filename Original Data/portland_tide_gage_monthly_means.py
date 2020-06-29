# -*- coding: utf-8 -*-
"""
Simple script to download Portland Tide Station data to CSV files.

This script dowloads monthly means. Minor modifications can provide access to a
wide variety of tide data, including six minute interval tidal elevations, tide
predictions, etc. A script is convenient for data access in part because 6 min
resolution data are only downloadable on a monthly basis.  This script manages
sequential downloads and assembly of a consistent data set.

The API is described here:
https://tidesandcurrents.noaa.gov/api/

@author: Curtis Bohlen, Casco Bay Estuary Partnership
@date:  June, 2020
"""
import requests

from datetime import date, datetime, timedelta
from time import time

MONTHLENGTHS = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
BASE = r'https://tidesandcurrents.noaa.gov/api/datagetter'

PARAMETERS = {
    'station':'8418150',   # Portland'', Maine
    'product':'monthly_mean',
    # other products for this station include':'
        # Water level            water_level
        # Air Temperature        air_temperature
        # Water  Temperature     water_temperature
        # Barometric Pressure    air_pressure
        # Predicted tides        predictions
        # datums  may also be useful to convert between units on the fly
    'application':'CascoBayEstuaryPartnership',  # This is just to be polite
    'begin_date':'20150101',    # express dates in yyyymmdd or mm/dd/yyyy format
    'end_date':'20150105',
    'datum':'NAVD',  # many alternatives. Mostl kely would be MLLW
    'units':'english',
    'time_zone':'gmt',   # This gives conventional clock time, with DST,  Alternatives: gmt or lst
    'format':'csv'  # also available are json and xml
    #interval = 6 min interval -- only need to specify hourly
    }

MAINESTATIONS = {#'Bar Harbor':'8413320',
                 #'Cutler':'8411060',
                 #'Eastport':'8410140',
                 'Portland':'8418150' #,
                 #'Wells': '8419317'
                 }
                 
def setupfile(thefilename):
    with open(thefilename, 'w') as outfile:
        outfile.write('DateTime, Prediction\n')
        
def adddata(thefilename, theresponse):
    with open(thefilename, 'a') as outfile:
        for a in theresponse.split('\n'):
            if a[:4] == 'Date':  #Eliminate the header row for each pass
                continue
            outfile.write(a +'\n')
            
if __name__ == '__main__':
    for thestation, thecode in MAINESTATIONS.items():
        thefile = thestation + '_SLR History.csv'
        setupfile(thefile)   # this will erase any file with the same name.

        PARAMETERS['station'] = thecode
        PARAMETERS['begin_date']= '19000101'
        PARAMETERS['end_date'] = '20171231'
        response = requests.get(BASE, params = PARAMETERS)
        adddata(thefile, response.text)
