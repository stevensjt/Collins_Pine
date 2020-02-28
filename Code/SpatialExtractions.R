####Historical Forest Structure extractions of underlying topography and climate

library(raster)
library(rgdal)
library(readxl)
library(tidyverse)
library(rgeos)
####El Dorado####
#Elevation bands
#eldo <- st_read("../Large Files/GIS/Eldo_Lotcodes.shp", quiet = TRUE) #If using package sf
eldo <- readOGR("./GIS/AllQQs_wHistData.shp") #EPSG 4269; NAD83
#eldo <- readOGR("../Large Files/GIS/El Dorado/Modern/ENF_FIA_subset.shp") #EPSG 4269; NAD83; Option to do for modern FIA data.
#START HERE, do extractions on FIA data
r <- raster("../Large Files/GIS/DEM/Eldo_DEM.tif") #EPSG 4269; NAD83. From viewer.nationalmap.gov; 1/3 arc sec (~10 m) resolution
elev_mean <- raster::extract(r, eldo, fun=mean) #13 minutes for 642 lots
eldo_elev <- #If doing for FIA, set id = eldo$CN and elev_extract = elev_mean
  data.frame(site = "eldo", id = eldo$Lotcode, elev_mean = elev_mean) 
Sys.time()
#write_csv(eldo_elev,"Data/Derived/eldo_elev.csv") #If doing for FIA, skip.

#Climate data
eldo <- readOGR("../Large Files/GIS/El Dorado/Eldo_Lotcodes_Active.shp") #EPSG 4269; NAD83
cwd <- raster("../Large Files/GIS/El Dorado/Climate/cwd1981_2010_ave_HST_1519244261/cwd1981_2010_ave_HST_1519244261.tif") #From http://climate.calcommons.org/node/1129 CA Climate Commons, BCM. 30 year water-year mean annual climatic water deficit values 1981-2010. EPSG 3310 NAD83 CA Albers
cwd_jun <- raster("../Large Files/GIS/El Dorado/Climate/cwd1981_2010jun_ave_HST_1519244398/cwd1981_2010jun_ave_HST_1519244398.tif") #Mean monthly climatic water deficit for June 1981-2010
aet <- raster("../Large Files/GIS/El Dorado/Climate/aet1981_2010_ave_HST_1519244035/aet1981_2010_ave_HST_1519244035.tif") #mean actual evapotranspiration values 
spk <- raster("../Large Files/GIS/El Dorado/Climate/aprpck1981_2010_ave_HST_1519244342/aprpck1981_2010_ave_HST_1519244342.tif") #mean April 1 snowpack values
ppt <- raster("../Large Files/GIS/El Dorado/Climate/ppt1981_2010_ave_HST_1519244286/ppt1981_2010_ave_HST_1519244286.tif") #mean annual precipitation values
tmx <- raster("../Large Files/GIS/El Dorado/Climate/tmx1981_2010_ave_HST_1519244138/tmx1981_2010_ave_HST_1519244138.tif") #Mean annual max temperatures 1981-2010.
#tmx_jja <- raster("../Large Files/GIS/El Dorado/Climate/tmx1981_2010jja_ave_HST_1505416830/tmx1981_2010jja_ave_HST_1505416830.tif") #Mean annual max temperatures 1981-2010. Not using.
slope <- raster("../Large Files/GIS/DEM/Eldo_Slope.tif") #EPSG 4269; NAD83. Derived from DEM in QGIS; Raster -> Analysis -> DEM (Terrain Models), Mode = slope, scale = 111120 (per http://learninggis.com/working-with-dem-models-qgis-quantum-gis).
aspect <- raster("../Large Files/GIS/DEM/Eldo_aspect.tif") #EPSG 4269; NAD83. Derived from DEM in QGIS; Raster -> Analysis -> DEM (Terrain Models), Mode = aspect


eldo_extractions <- read_csv("Data/Derived/eldo_elev.csv") #Tack on to elevation data
#eldo_extractions <- eldo_elev #If doing for FIA, set eldo_extractions <- eldo_elev (skipping the saving step above)
eldo_extractions$cwd_mean8110 <- raster::extract(cwd, eldo, fun=mean) #Transformation warnings ok
eldo_extractions$cwd_jun_mean8110 <- raster::extract(cwd_jun, eldo, fun=mean)
eldo_extractions$aet_mean8110 <- raster::extract(aet, eldo, fun=mean)
eldo_extractions$spk_mean8110 <- raster::extract(spk, eldo, fun=mean)
eldo_extractions$ppt_mean8110 <- raster::extract(ppt, eldo, fun=mean)
eldo_extractions$tmx_mean8110 <- raster::extract(tmx, eldo, fun=mean)
#eldo_extractions$tmx_jja_mean8110 <- raster::extract(tmx_jja, eldo, fun=mean) 
#tmx_jja is highly correlated with annual tmx, so not including

eldo_centroids <- rgeos::gCentroid(eldo,byid=TRUE)
eldo_extractions$slope <- raster::extract(slope, eldo, fun=mean)
eldo_extractions$aspect_num <- raster::extract(aspect, eldo_centroids, fun=max)
eldo_extractions$aspect_cat <- 
  ifelse(between(eldo_extractions$aspect_num, 135,315),"SW","NE")
#write_csv(eldo_extractions,"Data/Derived/eldo_extractions.csv")
#write_csv(eldo_extractions,"Data/Derived/eldo_FIA_extractions.csv") #If doing for FIA data

####Klamath####
#Elevation extraction
kir <- readOGR("../Large Files/GIS/Klamath/KIR_TPA_BA_NAD83.shp") #EPSG 4269; NAD83
r <- raster("../Large Files/GIS/Klamath/DEM/KIR_DEM.tif") #EPSG 4269; NAD83. Processed from from https://earthexplorer.usgs.gov/
#1 arc sec (~30 m) resolution.

Sys.time()
kir_elev_mean <- raster::extract(r, kir, fun=mean) #30 seconds for 100 transects; 63 minutes for 12612 transects
Sys.time()
kir_elev <- data.frame(site = "kir", id = kir$TRSQ_trans, elev_mean = kir_elev_mean)
write_csv(kir_elev,"Data/Derived/kir_elev.csv")
