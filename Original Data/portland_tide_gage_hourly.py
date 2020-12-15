# -*- coding: utf-8 -*-
"""
Quick script to download Portland Tide Station data to CSV files

A script is convenient because hourly data are only available on a
yearly basis.

@author: Curtis
"""
import requests

from datetime import date

MONTHLENGTHS = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
BASE = r'https://tidesandcurrents.noaa.gov/api/datagetter'

PARAMETERS = {
    'station':'8418150',   # Portland'', Maine
    'product':'hourly_height',
    # other products for this station include':'
        # Air Temperature        air_temperature
        # Water  Temperature     water_temperature
        # Barometric Pressure    air_pressure
        # Predicted tides        predictions
        # Observed Water Levels  water_level
        # Hourly heights         hourly_height (predates six minute data)
        # datums  may also be useful to convert between units on the fly
    'application':'CascoBayEstuaryPartnership',  # This is just to be polite
    'begin_date':'20150101',    # express dates in yyyymmdd or mm/dd/yyyy format
    'end_date':'20150105',
    'datum':'MLLW',  # many alternatives. Most likely would be MSL, MLLW or NAVD
    'units':'metric',
    'time_zone':'lst',   # This gives consistent time, without DST,  Alternatives: gmt or lst_dst
    'format':'csv',  # also available are json and xml
    'interval':'h' # only need to specify 'h', hourly, or 'hilo' 6 min is default
    }


def setupfile(thefilename):
    with open(thefilename, 'w') as outfile:
        outfile.write('DateTime, Date, Time, Water Level, Sigma\n')
        
def assembleurl(base, parms):
    theurl = base + '?'
    for k,v in parms.iteritems():
        theurl += k + '=' + v + '&'
    return theurl[:-1]  
        
def adddata(thefilename, theresponse):
    with open(thefilename, 'a') as outfile:
        for a in theresponse.split('\n'):
            if a[:4] == 'Date':  #Eliminate the header row for each pass
                continue
            if a[:6] == 'Error':  # Eliminate rows that say there's a problem
                continue
            if  a.find('1,1,1') == -1: # Eliminate rows that contain no real data because of errors
                try:
                    lst = a.split(',')
                    thedate, thetime = lst[0].split()
                except ValueError:  # If you can't parse the line, skip it!
                    continue
                outfile.write(lst[0] + ',' + thedate + ',' +
                              thetime + ',' + lst[1] + ',' + lst[2] + '\n')
if __name__ == '__main__':
    thefile = 'portland_tides_hourly.csv'
    setupfile(thefile)   # this will erase any file with the same name.
    for year in range(1912, 2020):
        print ('Year:', year)
        begindate = str(year) + '0101'
        enddate = str(year) + '1231'
        PARAMETERS['begin_date'] = begindate
        PARAMETERS['end_date'] = enddate
        #url = assembleurl(BASE, PARAMETERS)
        response = requests.get(BASE, PARAMETERS)
        adddata(thefile, response.text)