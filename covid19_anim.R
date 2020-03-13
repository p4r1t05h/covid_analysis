#Cleaning the environment
rm(list=ls())

#Loading required libraries
library(rgdal)
library(tmap)
library(reshape2)

#Loading the dataset 
#Data set downloaded from https://github.com/CSSEGISandData/COVID-19/
data=read.csv("~/Desktop/covid_analysis/time_series_covid_19_confirmed.csv")
data=data[,-53]

#Reahashing Data set
ds=melt(data, id.vars=c("Province.State", "Country.Region", "Lat", "Long"))

#Changing Date formats
ds$variable=format(as.Date(substr(ds$variable,2,5), format = '%m.%d'), "%d-%m-%Y")
ds$variable=as.Date(ds$variable, format = "%d-%m-%Y")

#Removing NA values
ds=ds[!is.na(ds$value),]

#Loading World Shape File
shp=readOGR('~/Desktop/covid_analysis/World_Countries/World_Countries.shp')

#Converting DataFrame to SpatialPoints DataFrame
sp=data.frame(ds$Long, ds$Lat)
names(sp)=c('lng', 'lat')
coordinates(sp) <- ~lng+lat
proj4string(sp) <- proj4string(shp)
sp <- spTransform(sp, proj4string(shp))
sp=SpatialPointsDataFrame(sp, ds[,5:6])

str(sp)

#Creating TMAP objects of all the data for different dates
pl=tm_shape(shp) +
  tm_polygons(lwd=0.08) +
  tm_shape(sp) + 
  tm_bubbles(size = 'value',col = 'value', palette='YlOrRd',
             breaks=c(1,10,100,250,500,750,1000,5000,10000,Inf),
             title.size="No of Confirmed Cases",
             title.col='Legend')+
  tm_layout(inner.margins = 0.05,
            main.title = paste('Movement of Corona Virus Around the World'),
            main.title.size = 1) +
  tm_legend(legend.outside=F,legend.position=c("left", "BOTTOM")) +
  tm_facets(by = 'variable', nrow=1,ncol=1)

#Creating and Saving the Animation
tmap_animation(pl, filename="Desktop/covid_analysis/covid_test.gif",
               width=2000, height = 1600, delay=60)
