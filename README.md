# Somass River temperature forecasting project

Time series analyses of Somass River temperature data. 
This project includes two main analyses:

## 1. Pre-season Forecasting 
This work focuses on identifying drivers of annual temperature patterns that may be related to the severity of in-river conditions throughout the summer.

The directory `clim` analyses relationships between large-scale climatic variables and summer conditions in the Somass River. These analyses identify weak relationships between average summer river temperatures and several climatic indices, including the [Pacific Northwest Index](https://www.cbr.washington.edu/dart/pni), [Mount Cokely snow pack data](https://aqrt.nrs.gov.bc.ca/Data/Location/Summary/Location/3B02A/Interval/Latest) and oceanographic indices courtesy of [NOAA](https://www.ncei.noaa.gov/access/monitoring/enso/sst). 

## 2. In-season Forecasting
The folder `weekly` models weekly river temperatures using seasonal terms and air temperature data. An autoregressive integrated moving average ([ARIMA](https://otexts.com/fpp2/arima.html)) model is used to characterize five years of time series data and forecast future river temperatures. 
