# Sources of Data
## Sea Level Rise in Portland, Maine

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

**8418150_meantrend.csv**  
Data downloaded directly from NOAA Tides and Currents (Using "Export to CSV"
button).
(https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8418150) 
June 6, 2020 by Curtis C. Bohlen

Data are expressed in 

Note:  The webpage declares the average sea level rise to be 1.89+/- 0.14 mm/yr
which is equivalent to a change of 0.62 feet in 100 years. 

**portland_tides_monthly.csv**  
Data downloaded directly from NOAA API using a simple python script
[(here)](portland_tide_gage_monthly.py).
Details on the API are available from the
[NOAA web page](https://tidesandcurrents.noaa.gov/api/).

Data is highly correlated, but not identical with the previous data set. After
review, this data was not used in analysis or preparation of graphics.

**portland_tides_hourly.csv**  
Data downloaded directly from NOAA API using a simple python script
[(here)](portland_tide_gage_hourly.py).
Details on the API are available from the
[NOAA web page](https://tidesandcurrents.noaa.gov/api/).

This data was used to generate daily counts of exceedances above flood levels,
where flood levels are defined as observations above Portland's Highest Astronomical Tide (HAT), at 11.95 MLLW, or 

**portland_tides_hourly_predicts.csv**  
Data downloaded directly from NOAA API using a simple python script
[(here)](portland_tide_gage_hourly_predicts.py).
Details on the API are available from the
[NOAA web page](https://tidesandcurrents.noaa.gov/api/).


