#required libraries
library(ncdf4)
library(xts)
library(tidyverse)
library(strucchange)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(forecast)
library(ggfortify)
library(reshape2)

##Data import
hadisst <- nc_open('data/HadISST_sst.nc')
ersst <- nc_open('data/sst.mnmean.nc')

#extracting variables(lon,lat,time) from the nc file
lon <- ncvar_get(hadisst,"lon")
lat <- ncvar_get(hadisst,"lat")
time <- ncvar_get(hadisst,"time")



