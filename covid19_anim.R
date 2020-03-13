rm(list=ls())

library(rgdal)
library(tmap)
library(reshape2)

data=read.csv("~/Downloads/time_series_covid_19_confirmed.csv")
data=data[,-53]

ds=melt(data, id.vars=c("Province.State", "Country.Region", "Lat", "Long"))

ds$variable=format(as.Date(substr(ds$variable,2,5), format = '%m.%d'), "%d-%m-%Y")
ds$variable=as.Date(ds$variable, format = "%d-%m-%Y")

ds=ds[!is.na(ds$value),]


shp=readOGR('~/Downloads/World_Countries/World_Countries.shp')

sp=data.frame(ds$Long, ds$Lat)
names(sp)=c('lng', 'lat')
coordinates(sp) <- ~lng+lat
proj4string(sp) <- proj4string(shp)
sp <- spTransform(sp, proj4string(shp))
sp=SpatialPointsDataFrame(sp, ds[,5:6])

str(sp)

pl=tm_shape(shp) +
  tm_polygons(lwd=0.08) +
  tm_shape(sp) + 
  tm_bubbles(size = 'value',col = 'value', palette='YlOrRd',
             # breaks=c(1,10,100,500,1000,10000,25000,50000,Inf),
             title.size="No of Confirmed Cases",
             title.col='Legend')+
  tm_layout(inner.margins = 0.05,
            main.title = paste('Movement of Corona Virus Around the World'),
            main.title.size = 1) +
  tm_legend(legend.outside=F,legend.position=c("left", "BOTTOM")) +
  tm_facets(by = 'variable', nrow=1,ncol=1)

tmap_animation(pl, filename="Desktop/covid_test.gif",
               width=2000, height = 1600, delay=60)
