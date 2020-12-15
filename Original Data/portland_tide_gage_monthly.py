# -*- coding: utf-8 -*-
"""
Quick script to download Portland Tide Station data to CSV files

A script is convenient because 6 min resolution data are only available on a
monthly basis.

@author: Curtis
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
        # Hourly heights         hourly_height (predates six minute data)
        # datums  may also be useful to convert between units on the fly
    'application':'CascoBayEstuaryPartnership',  # This is just to be polite
    'begin_date':'20150101',    # express dates in yyyymmdd or mm/dd/yyyy format
    'end_date':'20150105',
    'datum':'msl',  # many alternatives. Most likely would be MLLW or NAVD
    'units':'metric',  # Alternative is "english"
    'time_zone':'gmt', # This gives conventional clock time, with DST,  Alternatives: gmt or lst
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
            outfile.write(a +'\n')
            
if __name__ == '__main__':
    for thestation, thecode in MAINESTATIONS.items():
        thefile = thestation + '_SLR_Monthly.csv'

        PARAMETERS['station'] = thecode
        PARAMETERS['begin_date']= '19000101'
        PARAMETERS['end_date'] = '20171231'
        response = requests.get(BASE, params = PARAMETERS)
        adddata(thefile, response.text)