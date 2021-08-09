# sf, raster, tmap: the spatial data trinity
# AOS SCO R Lightning Symposium Presentation
# By: Matt Dyson

library(sf)
library(raster)
library(tmap)
library(tidyverse)


# sf ----------------------------------------------------------------------

#Load in a shapefile

#BCR download link
#https://www.birdscanada.org/bird-science/nabci-bird-conservation-regions/
  
bcr <- read_sf("spatialData/bcr_terrestrial_shape/BCR_Terrestrial_master.shp")

bcr

plot(bcr["BCRNAME"])

unique(bcr$PROVINCE_S)

bcrON <- bcr[bcr$PROVINCE_S=="ONTARIO",]

plot(bcrON["BCRNAME"])

bcrON_LGL <- bcrON[bcrON$BCRNAME=="LOWER GREAT LAKES/ ST. LAWRENCE PLAIN",]
bcrON_LGL <- bcrON_LGL[1:2,]

bcrON_LGL <- bcrON_LGL %>% st_transform("+proj=lcc +lat_0=0 +lon_0=-85 +lat_1=44.5 +lat_2=53.5 +x_0=930000 +y_0=6430000 +datum=NAD83 +units=m +no_defs")

plot(st_geometry(bcrON_LGL))

randSamp <-  st_sample(bcrON_LGL, 100, type="random")

plot(st_geometry(bcrON_LGL))
plot(randSamp, add=T)

randSampBuff <- st_buffer(randSamp, 1000)

plot(randSampBuff)

# raster ------------------------------------------------------------------

#Ontario Provincial Digital Elevation Model

#https://geohub.lio.gov.on.ca/maps/mnrf::provincial-digital-elevation-model-pdem/about

onDEM <- raster("spatialData/PDEM_South.tif")

onDEM

plot(onDEM)

st_crs(onDEM) == st_crs(bcrON_LGL)

plot(st_geometry(bcrON_LGL), add=T)

onDEM_LGL <- onDEM %>% crop(bcrON_LGL)

plot(onDEM_LGL)

#Extract DEM for buffered points

meanDEM <- raster::extract(onDEM_LGL, as_Spatial(randSampBuff), fun=mean)

sdDEM <- raster::extract(onDEM_LGL, as_Spatial(randSampBuff), fun=sd)

randSamp <- randSamp %>% st_as_sf()

randSamp2 <- cbind(randSamp=randSamp, meanDEM=meanDEM, sdDEM=sdDEM)

# tmap --------------------------------------------------------------------

library(viridis)

tmap_mode("plot")

#LGL Mean DEM
tm2 <- tm_shape(bcrON_LGL)+
  tm_fill(col="grey", alpha=0.5)+
  tm_shape(randSamp2)+
  tm_bubbles(col="meanDEM", palette=rev(viridis(10)), alpha=0.75,
             title.col="Mean DEM (m)")+
  tm_scale_bar(position = c("left","bottom"), text.size = 1)+
  tm_compass(position = c("right", "bottom"), size = 4)+
  tm_layout(frame=F, 
            legend.text.size = 1, 
            legend.title.size = 2)
tm2

#Interactive
tmap_mode("view")
#https://leaflet-extras.github.io/leaflet-providers/preview/

tm4 <- tm_basemap("Esri.WorldImagery")+
  tm_shape(onDEM)+
  tm_raster(palette=rev(viridis(10)), title="Ontario Digital Elevation",
            alpha=0.5)+
  tm_shape(bcrON)+
  tm_borders(col="black")+
  tm_shape(randSamp2)+
  tm_bubbles(col="meanDEM", palette=rev(viridis(10)), alpha=0.75,
             title.col="Mean DEM (m)", size=0.1)

tm4
 


