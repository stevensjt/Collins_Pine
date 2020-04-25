####Historical Forest Structure extractions of underlying topography and climate
#Author: Jens Stevens

library(raster)
library(rgdal)
library(readxl)
library(tidyverse)
library(rgeos)
library(glmulti) #For glmulti(); version 1.0.7
library(MuMIn) #For r.squaredGLMM(); version 1.43.6
library(rpart) #For rpart(); version 4.1-11
library(rpart.plot) #for prp(); version 2.1.2
library(RColorBrewer) #for brewer.pal(); version 1.1-2
library(grid) #for viewport(); version 3.4.3

####1. Spatial Extractions; only need to do once per user####
gis_path <- "./large_files/Lassen_GIS_JTS/"
#Note: Different users will need to specify their own GIS path.

#1a. Topo data
#lots <- readOGR("./GIS/AllQQs_wHistData.shp") #EPSG 3310 CA Albers; NAD83. Extractions for historical data. 
#plots <- readOGR("./GIS/AllFIA.shp"); lots <- plots #EPSG 3310 CA Albers; NAD83. Extractions for modern FIA data. n=76
  #Note FIA plot 484816538489998 ended up being outside of a slightly more restricted footprint (ask JTS).
#r <- raster(paste0(gis_path,"Topography/Collins DEM.tif")) #EPSG 3310 CA Albers; NAD83. From viewer.nationalmap.gov; 1/3 arc sec (~10 m) resolution

#elev_mean <- #Only run once
#  raster::extract(r, lots, fun=mean) #7 minutes for 1552 lots; fast for FIA
#lots_elev <- #Only run once, for QQ
#  data.frame(site = "lassen", id = lots$LotCodeFul, elev_mean = elev_mean) 
#plots_elev <- #Only run once, for FIA
#  data.frame(site = "lassen", id = plots$PLT_CN, elev_mean = elev_mean) 
#write_csv(lots_elev,"Data/Derived/lots_elev.csv") #only run once
#write_csv(plots_elev,"Data/Derived/plots_elev.csv") #only run once
#lassen_extractions <- #Set this to either lots_elev (for QQ extractions) or plots_elev (for FIA extractions)
#  read_csv("Data/Derived/plots_elev.csv")

#slope <- #EPSG 3310 CA Albers; NAD83. Derived from DEM in QGIS
#  raster(paste0(gis_path,"Topography/Collins Slope.tif")) 
#aspect <-  #EPSG 3310 CA Albers; NAD83. Derived from DEM in QGIS
#  raster(paste0(gis_path,"Topography/Collins Aspect.tif")) 
#lots_centroids <- rgeos::gCentroid(lots,byid=TRUE) #historical QQ lots only, not FIA plots
#Sys.time() #7 minutes for QQ, fast for FIA
#lassen_extractions$slope <- raster::extract(slope, lots, fun=mean)
#Sys.time() #0 minutes
#lassen_extractions$aspect_num <- raster::extract(aspect, lots_centroids, fun=max)
#lassen_extractions$aspect_cat <- 
#  ifelse(between(lassen_extractions$aspect_num, 135,315),"SW","NE")
#Sys.time()


#1b. Climate data
cwd <- #From http://climate.calcommons.org/node/1129 CA Climate Commons, BCM. 
  #30 year water-year mean annual climatic water deficit values 1981-2010. 
  #EPSG 3310 NAD83 CA Albers
  raster(paste0(gis_path,"Climate_Raw/cwd1981_2010_ave_HST_1575394138/cwd1981_2010_ave_HST_1575394138.tif")) 
cwd_jun <- #Mean monthly climatic water deficit for June 1981-2010
  raster(paste0(gis_path,"Climate_Raw/cwd1981_2010jun_ave_HST_1575394203/cwd1981_2010jun_ave_HST_1575394203.tif")) 
aet <- #mean actual evapotranspiration values 
  raster(paste0(gis_path,"Climate_Raw/aet1981_2010_ave_HST_1575394039/aet1981_2010_ave_HST_1575394039.tif"))
spk <- #mean April 1 snowpack values
  raster(paste0(gis_path,"Climate_Raw/aprpck1981_2010_ave_HST_1575394218/aprpck1981_2010_ave_HST_1575394218.tif"))
ppt <- #mean annual precipitation values
  raster(paste0(gis_path,"Climate_Raw/ppt1981_2010_ave_HST_1575394242/ppt1981_2010_ave_HST_1575394242.tif"))
tmx <- #Mean annual max temperatures 1981-2010.
  raster(paste0(gis_path,"Climate_Raw/tmx1981_2010_ave_HST_1575394259/tmx1981_2010_ave_HST_1575394259.tif"))
#tmx_jja <- raster(paste0(gis_path, "Climate_Raw/tmx1981_2010jja_ave_HST_1575394284/tmx1981_2010jja_ave_HST_1575394284.tif")) #Mean annual max temperatures 1981-2010. Not using.

#Sys.time() #Each takes about 4 minutes
#lassen_extractions$cwd_mean8110 <- raster::extract(cwd, lots, fun=mean) 
#lassen_extractions$cwd_jun_mean8110 <- raster::extract(cwd_jun, lots, fun=mean)
#lassen_extractions$aet_mean8110 <- raster::extract(aet, lots, fun=mean)
#lassen_extractions$spk_mean8110 <- raster::extract(spk, lots, fun=mean)
#lassen_extractions$ppt_mean8110 <- raster::extract(ppt, lots, fun=mean)
#lassen_extractions$tmx_mean8110 <- raster::extract(tmx, lots, fun=mean)
#lassen_extractions$tmx_jja_mean8110 <- raster::extract(tmx_jja, lots, fun=mean) 
#tmx_jja is highly correlated with annual tmx, so not including (Not sure if true for Lassen)

#write_csv(lassen_extractions,"Data/Derived/lassen_QQ_extractions.csv") #If doing for QQ lots
#write_csv(lassen_extractions,"Data/Derived/lassen_FIA_extractions.csv") #If doing for FIA data

####2. Full data assembly and descriptions####
#Note: This is for the historical data, eventually need to replicate this for the FIA data but have to figure out modeling first.
d <- read_excel("Data/CollinsPine_HistData_ForJens.xlsx", sheet = "Sheet1")
env.d <- read_csv("Data/Derived/lassen_QQ_extractions.csv")
env.d <- env.d[,-c(1,5)]
names(env.d)[which(names(env.d)=="aspect_cat")] <- "aspect"
env.d$aspect <- factor(env.d$aspect)
d <- merge.data.frame(d,env.d,by.x = "LotCodeFull", by.y = "id")

d_summary <- 
  d %>%
  summarise(Tot_TPA = mean(Tot_TPA, na.rm = T),
            BA_Tot = mean(TotLvBA_ac),
            Mean_elev = mean(elev_mean),
            Min_elev = round(min(elev_mean),0),
            Max_elev = round(max(elev_mean),0),
            Mean_precip = mean(ppt_mean8110),
            Mean_temp = mean(tmx_mean8110)
  )

d <- #Remove 30 rows that are missing climate data because they're under Lake Almanor
  d[-which(!complete.cases(d)),] 
d$pine_fraction <- rowSums(d[,c("SP_BAac","PP_BAac")])/rowSums(d[,5:10])
#hist(d$pine_fraction)

####3. Analysis####
vfirst <- which(names(d)=="elev_mean")
vlast <- which(names(d)=="tmx_mean8110")
potential_parms <- names(d[c(vfirst:vlast)])

###3a. All trees, density####

##historical data
m_tph <- glmulti(y="Tot_TPA", 
                  xr=potential_parms,
                  data=d,
                  level=1,method="h", plotty = FALSE) 
summary(m_tph@objects[[1]]) #Best model includes aspect, elev, slope, cwd mean, cwd jun, aet, spk, and precip (all vars except tmx). AIC 9029
#ranks based on pvals 1) cwd, 2) aet, 3) spk (1-3 all close), 4) ppt, 5) slope (4-5 close), 6) aspect, 7) elev, 8) cwd jun (6-8 all close)
summary(m_tph@objects[[4]]) #AIC 9031; includes cwd (1), aet (2), spk (3), ppt (4), slope (5), aspect (6), elev (7)
summary(m_tph@objects[[14]]) #AIC 9035; The model with the fewest strong predictors: slope, CWD, AET, SPK and PPT
#r.squaredGLMM(m_tph@objects[[1]])

#Let's try random forests
library(randomForest)
md <- d[complete.cases(d),c("Tot_TPA", potential_parms)]
rfm_tph <- randomForest(Tot_TPA ~ ., data=md, importance = TRUE)
print(rfm_tph)
importance(rfm_tph) 
#ranks: 1) ppt (clearly), 2) elev, 3) aet, 4) slope (2-4 all close), 5) cwd (clearly), 6)cwd_jun, 7) spk, 8) tmx (6-8 close), 9) aspet


##Model 4 results
tree <- #Decide which model with which to create regression tree 
  #Using 4 because fewer parameters:
  rpart(m_tph@objects[[4]]$formula, 
        method = "anova",
        data = d,
        control = rpart.control(cp = 0.02))
levels(tree$frame$var) <- c("<leaf>", "deficit", "elevation" , "precip", "snowpack")
  
h <- #Histogram
  ggplot(d)+
  geom_histogram(aes(Tot_TPA), binwidth = 5, center = 2.5,
                  fill=c(rep(brewer.pal(9,"RdYlBu")[1],2),
                        rep(brewer.pal(9,"RdYlBu")[2],1),
                        rep(brewer.pal(9,"RdYlBu")[3],1),
                        rep(brewer.pal(9,"RdYlBu")[5],1),
                        rep(brewer.pal(9,"RdYlBu")[7],1),
                        rep(brewer.pal(9,"RdYlBu")[9],4)
                        ),
                  col="black")+
  labs(x = "TPA", y = "Count")+
  coord_cartesian(xlim = c(0, 60)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))
  
#cairo_pdf("./Figures/EDA/Model4.pdf", height = 4, width = 6.69)
prp(tree, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1, cex = 0.6, 
    box.col = brewer.pal(9,"RdYlBu")[c(2,7,3,5,3,5)],
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Historical tree density (TPA) \n(Model 4: elev, slope, aspect, CWD, AET, PPT and snowpack)")
print(h, vp=viewport(.78, .75, .2, .3))
print(grid.text("M4", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ) )
#dev.off()

##Model 14 results
tree <- #Decide which model with which to create regression tree 
  #Using 14 because fewest parameters: slope, deficit, AET, Snowpack and precip
  rpart(m_tph@objects[[14]]$formula, 
        method = "anova",
        data = d,
        control = rpart.control(cp = 0.02))
levels(tree$frame$var) <- c("<leaf>", "AET", "deficit", "precip")


#cairo_pdf("./Figures/EDA/Model14.pdf", height = 4, width = 6.69)
prp(tree, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1, cex = 0.6, 
    box.col = brewer.pal(9,"RdYlBu")[c(3,3,3,5,3)],
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Historical tree density (TPA) \n(Model 14: slope, deficit, AET, Snowpack and precip)")
print(h, vp=viewport(.78, .15, .2, .3))
print(grid.text("M14", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ) )
#dev.off()

m_14_pred <- m_tph@objects[[14]]$model
tpa_obs <- m_14_pred$Tot_TPA #store actual TPA for use later
m_14_pred$Tot_TPA <- NA
m_14_pred$Tot_TPA_pred <- predict.glm(m_tph@objects[[14]],newdata = m_14_pred)
m_14_pred$Tot_TPA_obs <- tpa_obs
#Check predictive power; not great
plot(m_14_pred$Tot_TPA_pred ~ m_14_pred$Tot_TPA_obs)
abline(0,1)

####3b. Basal Area####

####3c. Pine Fraction####

####4. Predictive Maps####
summary(m_tph@objects[[4]])
summary(m_tph@objects[[14]])
#Used "Align Rasters" tool in QGIS to create the below layers
  

aet <- raster(paste0(gis_path,"/Aligned/aet_aligned.tif"))
spk <- raster(paste0(gis_path,"/Aligned/aprpck_aligned.tif"))
asp <- raster(paste0(gis_path,"/Aligned/aspect_aligned.tif"))
cwd <- raster(paste0(gis_path,"/Aligned/cwd_aligned.tif"))
cwd_jun <- raster(paste0(gis_path,"/Aligned/cwd_jun_aligned.tif"))
dem <- raster(paste0(gis_path,"/Aligned/dem_aligned.tif"))
ppt <- raster(paste0(gis_path,"/Aligned/ppt_aligned.tif"))
slp <- raster(paste0(gis_path,"/Aligned/slope_aligned.tif"))
tmx <- raster(paste0(gis_path,"/Aligned/tmx_aligned.tif"))

#asp <- raster("../Large Files/GIS/El Dorado/AlignedLayers/asp_Align.tif") #Aspect in degrees; resampled to 270m res.
  
m_tph@objects[[14]]$formula
s <- stack(slp,cwd,aet,spk,ppt)
df <- as.data.frame(getValues(s))
df[df<0] <- NA
r <- 1
for(r in 1:nrow(df)){
  if(any(is.na(df[r,]))){
    df[r,] <- NA
  }
}
names_tmp <- names(df)
names(df) <- c("slope", "cwd_mean8110", "aet_mean8110", "spk_mean8110", "ppt_mean8110")
#df$aspect_cat <- 
#  ifelse(between(df$aspect, 135,315),"SW","NE")
#df$aspect <- factor(df$aspect_cat)

df$pred_tph <- predict.lm(m_tph@objects[[14]], newdata = df)

#Exclude areas not within the climate envelope of the data
df[which(!between(df$cwd_mean8110,range(d$cwd_mean8110)[1],range(d$cwd_mean8110)[2])), ] <-
  NA
df[which(!between(df$aet_mean8110,range(d$aet_mean8110)[1],range(d$aet_mean8110)[2])), ] <-
  NA
df[which(!between(df$spk_mean8110,range(d$spk_mean8110)[1],range(d$spk_mean8110)[2])), ] <-
  NA
df[which(!between(df$ppt_mean8110,range(d$ppt_mean8110)[1],range(d$ppt_mean8110)[2])), ] <-
  NA
df[which(!between(df$slope,range(d$slope)[1],range(d$slope)[2])), ] <-
  NA
#df[which(!between(df$slope_mean8110,range(d$slope_mean8110)[1],range(d$slope_mean8110)[2])), ] <-NA
#df[which(!between(df$aspect_mean8110,range(d$aspect_mean8110)[1],range(d$aspect_mean8110)[2])), ] <-NA
df[which(df$pred_tph<1), ] <-NA #Model fit gives some negative values at margins still. 


pred_Align <- cwd
pred_Align <- setValues(x = pred_Align, values = df$pred_tph)
range(getValues(pred_Align), na.rm = T); range(df$pred_tph, na.rm = T)
plot(pred_Align)

#writeRaster(pred_Align, "GIS/TPH_Predicted.tif", overwrite = FALSE)
  
  