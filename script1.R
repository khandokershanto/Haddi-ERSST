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
lon <- ncvar_get(hadisst,"longitude")
lat <- ncvar_get(hadisst,"latitude")
time <- ncvar_get(hadisst,"time")

#numeric to Date format
time <- as.Date(time,origin="1870-1-1 0:0:0",tz="UTC")

#set limit to time,lon,lat

lon_lim <- c(80,100)
lat_lim <- c(5,25)
time_lim <- c(ymd("1920-01-01"),ymd("2019-12-01"))

lon_ind <- which(lon >= lon_lim[1] & lon <= lon_lim[2])
lat_ind <- which(lat >= lat_lim[1] & lat <= lat_lim[2])
time_ind <- which(time >= time_lim[1] & time <= time_lim[2]) 

#extract the sst from the limited time,lon,lat
sst <- ncvar_get(hadisst,"sst",start = c(lon_ind[1],lat_ind[1],time_ind[1]),count = c(length(lon_ind),length(lat_ind),length(time_ind)))

#time series by averaging lon,lat,time
hadi_ts <- apply(sst,3,mean,na.rm=TRUE)
plot.ts(hadi_ts)

#make time series
ts1 <- ts(hadi_ts,start = 1920,end = 2019,frequency = 12)

png('monthly_hadiSST.png',width = 6,height = 4,units = 'in',res = 300)
plot(ts,xlab = 'SST',ylab = 'Year')
dev.off()




###################ERSST

#data
ersst <- nc_open('data/sst.mnmean.nc')

#extracting variables(lon,lat,time) from the nc file
lon <- ncvar_get(ersst,"lon")
lat <- ncvar_get(ersst,"lat")
time <- ncvar_get(ersst,"time")

#numeric to Date format
time <- as.Date(time,origin="1800-1-1 00:00:00",tz="UTC")

#set limit to time,lon,lat

lon_lim <- c(80,100)
lat_lim <- c(5,25)
time_lim <- c(ymd("1920-01-01"),ymd("2019-12-01"))

lon_ind <- which(lon >= lon_lim[1] & lon <= lon_lim[2])
lat_ind <- which(lat >= lat_lim[1] & lat <= lat_lim[2])
time_ind <- which(time >= time_lim[1] & time <= time_lim[2]) 

#extract the sst from the limited time,lon,lat
sst <- ncvar_get(ersst,"sst",start = c(lon_ind[1],lat_ind[1],time_ind[1]),count = c(length(lon_ind),length(lat_ind),length(time_ind)))

#time series by averaging lon,lat,time
er_ts <- apply(sst,3,mean,na.rm=TRUE)
plot.ts(er_ts)


##
year <- time[time_ind] #hadi
year_ <- time[time_ind] #ersst

plot(year,hadi_ts,type = 'l')

## time series making for decomposition
ts1 <- ts(hadi_ts,start = c(1920,1),frequency = 12)
plot(ts1)
####

ts2 <- ts(er_ts[-1200],start = c(1920,1),frequency = 12)

#corelation  between ersst & hadISST
cor(ts1,ts2) #highly corelated  
# corelation coefficient 0.9711249

#  seasonally adjusted

decomposed <- decompose(ts1, type = "additive")
sadj_hadi <- ts1 - decomposed$seasonal

windows(width = 6,height = 4)
plot(sadj_hadi)

decomposed <- decompose(ts2, type = "additive")
sadj_er <- ts2 - decomposed$seasonal

windows(width = 6,height = 4)
plot(sadj_er,col = 'steelblue4')
lines(sadj_hadi,col='red4')







