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
#gis_path <- "./large_files/Lassen_GIS_JTS/"
#Note: Different users will need to specify their own GIS path.

#1a. Topo data
#r <- raster(paste0(gis_path,"Topography/Collins DEM.tif")) #EPSG 3310 CA Albers; NAD83. From viewer.nationalmap.gov; 1/3 arc sec (~10 m) resolution
#plots <- readOGR("./GIS/d_FIA.shp"); lots <- Extractions for modern FIA data. n=71
#plots <- spTransform(plots, CRS("+init=epsg:3310")) #Matching crs of FIA data to match raster datasets
#Note FIA plot 484816538489998 ended up being outside of a slightly more restricted footprint (ask JTS).
#lots <- readOGR("./GIS/SCascadeQQs_wHistData.shp") #New shapefile provided by Brandon with QQs from Plumas removed (EPSG 3310)

#elev_mean <- #Only run once
#raster::extract(r, plots, fun=mean) #7 minutes for 1552 lots; fast for FIA
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
#lassen_extractions$slope <- raster::extract(slope, plots, fun=mean)
#Sys.time() #0 minutes
#lassen_extractions$aspect_num <- raster::extract(aspect, plots, fun=max)
#lassen_extractions$aspect_cat <- 
#  ifelse(between(lassen_extractions$aspect_num, 135,315),"SW","NE")
#Sys.time()


#1b. Climate data
#cwd <- #From http://climate.calcommons.org/node/1129 CA Climate Commons, BCM. 
  #30 year water-year mean annual climatic water deficit values 1981-2010. 
  #EPSG 3310 NAD83 CA Albers
#  raster(paste0(gis_path,"Climate_Raw/cwd1981_2010_ave_HST_1575394138/cwd1981_2010_ave_HST_1575394138.tif")) 
#cwd_jun <- #Mean monthly climatic water deficit for June 1981-2010
#  raster(paste0(gis_path,"Climate_Raw/cwd1981_2010jun_ave_HST_1575394203/cwd1981_2010jun_ave_HST_1575394203.tif")) 
#aet <- #mean actual evapotranspiration values 
#  raster(paste0(gis_path,"Climate_Raw/aet1981_2010_ave_HST_1575394039/aet1981_2010_ave_HST_1575394039.tif"))
#spk <- #mean April 1 snowpack values
#  raster(paste0(gis_path,"Climate_Raw/aprpck1981_2010_ave_HST_1575394218/aprpck1981_2010_ave_HST_1575394218.tif"))
#ppt <- #mean annual precipitation values
#  raster(paste0(gis_path,"Climate_Raw/ppt1981_2010_ave_HST_1575394242/ppt1981_2010_ave_HST_1575394242.tif"))
#tmx <- #Mean annual max temperatures 1981-2010.
#  raster(paste0(gis_path,"Climate_Raw/tmx1981_2010_ave_HST_1575394259/tmx1981_2010_ave_HST_1575394259.tif"))
#tmx_jja <- raster(paste0(gis_path, "Climate_Raw/tmx1981_2010jja_ave_HST_1575394284/tmx1981_2010jja_ave_HST_1575394284.tif")) #Mean annual max temperatures 1981-2010. Not using.

#Sys.time() #Each takes about 4 minutes
#lassen_extractions$cwd_mean8110 <- raster::extract(cwd, plots, fun=mean) 
#lassen_extractions$cwd_jun_mean8110 <- raster::extract(cwd_jun, plots, fun=mean)
#lassen_extractions$aet_mean8110 <- raster::extract(aet, plots, fun=mean)
#lassen_extractions$spk_mean8110 <- raster::extract(spk, plots, fun=mean)
#lassen_extractions$ppt_mean8110 <- raster::extract(ppt, plots, fun=mean)
#lassen_extractions$tmx_mean8110 <- raster::extract(tmx, plots, fun=mean)
#lassen_extractions$tmx_jja_mean8110 <- raster::extract(tmx_jja, plots, fun=mean) 
#tmx_jja is highly correlated with annual tmx, so not including (Not sure if true for Lassen)

#write_csv(lassen_extractions,"Data/Derived/lassen_QQ_extractions.csv") #If doing for QQ lots
#write_csv(lassen_extractions,"Data/Derived/lassen_FIA_extractions.csv") #If doing for FIA data

####2. Full data assembly and descriptions####
#Note: This is for the historical data, eventually need to replicate this for the FIA data but have to figure out modeling first.

#Import new subset of QQ plots provided by Brandon (removed plots in Plumas), n = 1481
#Subsetting column with plot number (Lot) only to subset JTS historical data worksheet
new.qq <- read_xlsx("Data/SCasc_QQonly.xlsx") %>%
  select(LotCodeFull)

#Joined new subset of QQ data with existing QQ dataset
#Converted units from TPA to TPH and BA (sq ft/acre) to metric BA (sq m/ha)
#Estimated pine fraction (a bit different than 2018 paper; divided by total live BA)
d <- read_excel("Data/CollinsPine_HistData_ForJens.xlsx", sheet = "Sheet1") %>%
  inner_join(new.qq, ., by = "LotCodeFull") %>%
  mutate(Tot_TPH = Tot_TPA/0.404686,
         SP_BAmet = SP_BAac*0.229568,
         PP_BAmet = PP_BAac*0.229568,
         RF_BAmet = RF_BAac*0.229568,
         DF_BAmet = DF_BAac*0.229568,
         WF_BAmet = WF_BAac*0.229568,
         IC_BAmet = IC_BAac*0.229568,
         TotLvBA_met = SP_BAmet + PP_BAmet + RF_BAmet + DF_BAmet + WF_BAmet + IC_BAmet,
         pine_fraction = (SP_BAmet + PP_BAmet)/TotLvBA_met)
env.d <- read_csv("Data/Derived/lassen_QQ_extractions.csv")
env.d <- env.d[,-c(1,5)]
names(env.d)[which(names(env.d)=="aspect_cat")] <- "aspect"
env.d$aspect <- factor(env.d$aspect)
d <- merge.data.frame(d,env.d,by.x = "LotCodeFull", by.y = "id")

d <- #Remove 30 rows that are missing climate data because they're under Lake Almanor
  d[-which(!complete.cases(d)),]

#To make comparisons with FIA data, removing plots with < 9 Total live BA
d <- d%>%
  filter(TotLvBA_met > 9)

#Summary of historical data
d_summary <- 
  d %>%
  summarise(mean_TPH = mean(Tot_TPH),
            min_TPH = min(Tot_TPH),
            max_TPH = max(Tot_TPH),
            BAmet_Tot = mean(TotLvBA_met),
            BAmet_min = min(TotLvBA_met),
            BAmet_max = max(TotLvBA_met),
            Mean_pine_fraction = mean(pine_fraction),
            min_pine = min(pine_fraction),
            max_pine = max(pine_fraction),
            Mean_elev = mean(elev_mean),
            Min_elev = round(min(elev_mean),0),
            Max_elev = round(max(elev_mean),0),
            Mean_precip = mean(ppt_mean8110),
            min_precip = min(ppt_mean8110),
            max_precip = max(ppt_mean8110),
            Mean_temp = mean(tmx_mean8110),
            min_tmx = min(tmx_mean8110),
            max_tmx = max(tmx_mean8110),
            mean_cwd = mean(cwd_mean8110),
            min_cwd = min(cwd_mean8110),
            max_cwd = max(cwd_mean8110),
            mean_slope = mean(slope),
            min_slope = min(slope),
            max_slope = max(slope))

####3. Analysis####
#Checking correlations amongst variables to see if we can eliminate some
library(PerformanceAnalytics)
correlation.data <- d %>%
  select(21:30,
         -aspect)
corr.chart <- chart.Correlation(correlation.data, histogram = TRUE, pch = 19) #Excluding variables >= 0.7
#Just going to keep max temp, slope, aspect, mean cwd, and precipitation
model_parms5 <- names(d[c("tmx_mean8110", "slope", "aspect", "cwd_mean8110", "ppt_mean8110")])

###3a. All trees, density####

#Historical data with RandomForest
library(randomForest)

#RF with all 5 parameters
md5 <- d[complete.cases(d),c("Tot_TPH", model_parms5)]
set.seed(10) #To reproduce bootstrapping
rfm_tph5 <- randomForest(Tot_TPH ~ ., data=md5, importance = TRUE)
print(rfm_tph5)
varImpPlot(rfm_tph5) #Removing aspect

#RF with 4 parameters
tph_parms4 <- names(d[c("tmx_mean8110", "slope", "cwd_mean8110", "ppt_mean8110")])
md4 <- d[complete.cases(d),c("Tot_TPH", tph_parms4)]
set.seed(10) #To reproduce bootstrapping
rfm_tph4 <- randomForest(Tot_TPH ~ ., data=md4, importance = TRUE)
print(rfm_tph4)
varImpPlot(rfm_tph4) #Removing slope

#RF with 3 parameters
tph_parms3 <- names(d[c("tmx_mean8110", "cwd_mean8110", "ppt_mean8110")])
md3 <- d[complete.cases(d),c("Tot_TPH", tph_parms3)]
set.seed(10) #To reproduce bootstrapping
rfm_tph3 <- randomForest(Tot_TPH ~ ., data=md3, importance = TRUE)
print(rfm_tph3)
varImpPlot(rfm_tph3) #Removing cwd

#RF with 2 parameters
tph_parms2 <- names(d[c("tmx_mean8110", "ppt_mean8110")])
md2 <- d[complete.cases(d),c("Tot_TPH", tph_parms2)]
rfm_tph2 <- randomForest(Tot_TPH ~ ., data=md2, importance = TRUE)
print(rfm_tph2)
varImpPlot(rfm_tph2) #Removing ppt

#Comparing Random Forests
#Predictions from RandomForest
d$tph5 <- predict(rfm_tph5, d)
d$tph4 <- predict(rfm_tph4, d)
d$tph3 <- predict(rfm_tph3, d)
d$tph2 <- predict(rfm_tph2, d)

#Funciton to estimate RMSE
rmseFun <- function(a, b)
{rmse <- sqrt(mean((a - b)^2))}

#RMSE of TPH models
error.summary.tph <- d %>%
  summarise(rmse5 = rmseFun(tph5, Tot_TPH),
            rmse4 = rmseFun(tph4, Tot_TPH),
            rmse3 = rmseFun(tph3, Tot_TPH),
            rmse2 = rmseFun(tph2, Tot_TPH))
#Going with tph4 since it has highest %var explained and lowest RMSE

##CART for historical tph
h.tph <- #Histogram
  ggplot(d)+
  geom_histogram(aes(Tot_TPH), binwidth = 5, center = 2.5,
                 fill=c(rep(brewer.pal(9,"RdYlBu")[1],3),
                        rep(brewer.pal(9,"RdYlBu")[2],4),
                        rep(brewer.pal(9,"RdYlBu")[3],4),
                        rep(brewer.pal(9,"RdYlBu")[5],4),
                        rep(brewer.pal(9,"RdYlBu")[7],4),
                        rep(brewer.pal(9,"RdYlBu")[9],3)
                 ),
                 col="black")+
  labs(x = "TPH", y = "Count")+
  coord_cartesian(xlim = c(0, 125)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

#CART using same variables as tph4
tph.cart <- rpart(Tot_TPH ~ tmx_mean8110 +
                    ppt_mean8110 + 
                    cwd_mean8110 + 
                    slope,
        method = "anova",
        data = d,
        control = rpart.control(cp = 0.02))

#Relabeling variables for CART plot
levels(tph.cart$frame$var) <- c("<leaf>", "CWD", "precipitation", "max temperature")
  
#CART plot of historical TPH and inset of tph histogram
#Saved ("Figures/Lassen-Plumas/QQ_tph_cart.png)
prp(tph.cart, varlen = 0, faclen = 0, type = 3, extra = 1, cex = 0.6, 
    box.palette =  c("#f46d43", "#f46d43", "#fdae61"),
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Historical (1924) tree density \n(TPH ~ max temperature, precipitation, CWD, slope)")
print(h.tph, vp=viewport(.82, .75, .3, .3))
print(grid.text("a", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ) )
#dev.off

##modern comparison
#Getting FIA spatial extractions and plot data
#Renaming variables to join FIA data and match QQ data
fia_extractions <- read.csv("Data/Derived/lassen_FIA_extractions.csv") %>%
  rename(PLT_CN = id)

d_fia <- read.csv("Data/Derived/d_fia.csv") %>%
  rename(TotLvBA_met = BALIVE_sqmha,
         Tot_TPH = DENS_12plus) %>%
  inner_join(., fia_extractions, by = "PLT_CN")

d_fia_summary <- 
  d_fia %>%
  summarise(mean_TPH = mean(Tot_TPH),
            min_TPH = min(Tot_TPH),
            max_TPH = max(Tot_TPH),
            BAmet_Tot = mean(TotLvBA_met),
            BAmet_min = min(TotLvBA_met),
            BAmet_max = max(TotLvBA_met),
            Mean_pine_fraction = mean(pine_fraction),
            min_pine = min(pine_fraction),
            max_pine = max(pine_fraction),
            Mean_elev = mean(ELEV_met),
            Min_elev = round(min(ELEV_met),0),
            Max_elev = round(max(ELEV_met),0),
            Mean_precip = mean(ppt_mean8110),
            min_precip = min(ppt_mean8110),
            max_precip = max(ppt_mean8110),
            Mean_temp = mean(tmx_mean8110),
            min_tmx = min(tmx_mean8110),
            max_tmx = max(tmx_mean8110),
            mean_cwd = mean(cwd_mean8110),
            min_cwd = min(cwd_mean8110),
            max_cwd = max(cwd_mean8110),
            mean_slope = mean(slope),
            min_slope = min(slope),
            max_slope = max(slope))

#Applying breakpoints from CART to FIA data
{
  tree_fia <- tph.cart
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$tmx_mean8110 < 15, "Tot_TPH"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$tmx_mean8110 < 15, "Tot_TPH"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$tmx_mean8110 >= 15 & 
                                d_fia$ppt_mean8110 < 1019, "Tot_TPH"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$tmx_mean8110 >= 15 &
                 d_fia$ppt_mean8110 < 1019, "Tot_TPH"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$tmx_mean8110 >= 15 & 
                                d_fia$ppt_mean8110 >= 1019 & 
                                d_fia$ppt_mean8110 >= 1179 & 
                                d_fia$cwd_mean8110 < 517, "Tot_TPH"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$tmx_mean8110 >= 15 &
                 d_fia$ppt_mean8110 >= 1019 & 
                 d_fia$ppt_mean8110 >= 1179 & 
                 d_fia$cwd_mean8110 < 517, "Tot_TPH"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$tmx_mean8110 >= 15 & 
                                d_fia$ppt_mean8110 >= 1019 & 
                                d_fia$ppt_mean8110 >= 1179 & 
                                d_fia$cwd_mean8110 >= 517, "Tot_TPH"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$tmx_mean8110 >= 15 & 
                 d_fia$ppt_mean8110 >= 1019 & 
                 d_fia$ppt_mean8110 >= 1179 & 
                 d_fia$cwd_mean8110 >= 517, "Tot_TPH"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$tmx_mean8110 >= 15 & 
                                d_fia$ppt_mean8110 >= 1019 & 
                                d_fia$ppt_mean8110 < 1179, "Tot_TPH"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$tmx_mean8110 >= 15 &
                 d_fia$ppt_mean8110 >= 1019 &
                 d_fia$ppt_mean8110 < 1179, "Tot_TPH"], na.rm = T)
}

h_fia_tph <- #Histogram
  ggplot(d_fia)+
  geom_histogram(aes(Tot_TPH), binwidth = 15, center = 7.5,
                 fill=c(rep(brewer.pal(9,"RdYlBu")[1],5),
                        rep(brewer.pal(9,"RdYlBu")[2],5),
                        rep(brewer.pal(9,"RdYlBu")[3],4),
                        rep(brewer.pal(9,"RdYlBu")[5],4),
                        rep(brewer.pal(9,"RdYlBu")[7],4),
                        rep(brewer.pal(9,"RdYlBu")[8],5),
                        rep(brewer.pal(9,"RdYlBu")[9],5)
                 ),
                 col="black")+
  labs(x = "TPH", y = "Count")+
  coord_cartesian(xlim = c(0, 500)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

#CART plot of FIA TPH and inset of tph histogram
#Saved ("Figures/Lassen-Plumas/FIA_tph_cart.png)
prp(tree_fia, varlen = 0, faclen = 0, type = 3, extra = 1, cex = 0.6,
    box.palette =  c("#fdae61", "#f46d43", "#fdae61"),
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Modern (2011 - 2018) tree density \n(TPH ~ max temperature, precipitation, CWD, slope)")
print(h_fia_tph, vp=viewport(.82, .75, .3, .3))
print(grid.text("b", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ) )
#dev.off()

####3b. Basal Area####

#Historical data with RandomForest
#RF with all 5 parameters
bd5 <- d[complete.cases(d),c("TotLvBA_met", model_parms5)]
set.seed(10) #To reproduce bootstrapping
rfm_ba5 <- randomForest(TotLvBA_met ~ ., data=bd5, importance = TRUE)
print(rfm_ba5)
varImpPlot(rfm_ba5) #Removing aspect

#RF with 4 parameters
ba_parms4 <- names(d[c("tmx_mean8110", "slope", "cwd_mean8110", "ppt_mean8110")])
bd4 <- d[complete.cases(d),c("TotLvBA_met", ba_parms4)]
set.seed(10) #To reproduce bootstrapping
rfm_ba4 <- randomForest(TotLvBA_met ~ ., data=bd4, importance = TRUE)
print(rfm_ba4)
varImpPlot(rfm_ba4) #Removing slope

#RF with 3 parameters
ba_parms3 <- names(d[c("tmx_mean8110", "cwd_mean8110", "ppt_mean8110")])
bd3 <- d[complete.cases(d),c("TotLvBA_met", ba_parms3)]
set.seed(10) #To reproduce bootstrapping
rfm_ba3 <- randomForest(TotLvBA_met ~ ., data=bd3, importance = TRUE)
print(rfm_ba3)
varImpPlot(rfm_ba3) #Removing cwd

#RF with 2 parameters
ba_parms2 <- names(d[c("tmx_mean8110", "ppt_mean8110")])
bd2 <- d[complete.cases(d),c("TotLvBA_met", ba_parms2)]
set.seed(10) #To reproduce bootstrapping
rfm_ba2 <- randomForest(TotLvBA_met ~ ., data=bd2, importance = TRUE)
print(rfm_ba2)
varImpPlot(rfm_ba2)

#Comparing Random Forests
#Predictions from RandomForest
d$ba5 <- predict(rfm_ba5, d)
d$ba4 <- predict(rfm_ba4, d)
d$ba3 <- predict(rfm_ba3, d)
d$ba2 <- predict(rfm_ba2, d)

#RMSE of BA models
error.summary.ba <- d %>%
  summarise(rmse5 = rmseFun(ba5, TotLvBA_met),
            rmse4 = rmseFun(ba4, TotLvBA_met),
            rmse3 = rmseFun(ba3, TotLvBA_met),
            rmse2 = rmseFun(ba2, TotLvBA_met))
#Going with ba4 since it has highest %var explained and lowest RMSE

##CART for historical Live BA
h.ba <- #Histogram
  ggplot(d)+
  geom_histogram(aes(TotLvBA_met), binwidth = 5, center = 2.5,
                 fill=c(rep(brewer.pal(9,"RdYlBu")[1],2),
                        rep(brewer.pal(9,"RdYlBu")[2],1),
                        rep(brewer.pal(9,"RdYlBu")[3],1),
                        rep(brewer.pal(9,"RdYlBu")[5],1),
                        rep(brewer.pal(9,"RdYlBu")[7],1),
                        rep(brewer.pal(9,"RdYlBu")[9],2)
                 ),
                 col="black")+
  labs(x = "Total Live BA", y = "Count")+
  coord_cartesian(xlim = c(0, 50)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

#CART
ba.cart <- rpart(TotLvBA_met ~ tmx_mean8110 + 
                   ppt_mean8110 +
                   cwd_mean8110 + 
                   slope, 
                  method = "anova",
                  data = d,
                  control = rpart.control(cp = 0.02))

#Relabeling variables for CART plot
levels(ba.cart$frame$var) <- c("<leaf>", "CWD", "max temperature")

#CART plot of historical live BA
#Saved ("Figures/Lassen-Plumas/QQ_ba_cart.png)
prp(ba.cart, varlen = 0, faclen = 0, type = 3, extra = 1, cex = 0.6, 
    box.palette =  c("#f46d43", "#fdae61"),
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Historical (1924) live basal area \n(Live BA ~ max temperature, precipitation, CWD, slope)")
print(h.ba, vp=viewport(.8, .75, .4, .3))
print(grid.text("a", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ))#dev.off()

#Applying breakpoints from CART to FIA data
{
  tree_fia <- ba.cart
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$tmx_mean8110 < 15, "TotLvBA_met"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$tmx_mean8110 < 15, "TotLvBA_met"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$tmx_mean8110 >= 15 & 
                                d_fia$cwd_mean8110 < 449, "TotLvBA_met"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$tmx_mean8110 >= 15 & 
                 d_fia$cwd_mean8110 < 436, "TotLvBA_met"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$tmx_mean8110 >= 15 & 
                                d_fia$cwd_mean8110 >= 436, "TotLvBA_met"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$tmx_mean8110 >= 15 & 
                 d_fia$cwd_mean8110 >= 436, "TotLvBA_met"], na.rm = T)
}

#No trees match criteria for leaf 2 (elev < 1661 & CWD < 434); chaning NAN to 0
tree_fia[[1]][5][is.na(tree_fia[[1]][5])] <- 0

h_fia_ba <- #Histogram
  ggplot(d_fia)+
  geom_histogram(aes(TotLvBA_met), binwidth = 10, center = 2.5,
                 fill=c(rep(brewer.pal(9,"RdYlBu")[1],2),
                        rep(brewer.pal(9,"RdYlBu")[2],1),
                        rep(brewer.pal(9,"RdYlBu")[3],1),
                        rep(brewer.pal(9,"RdYlBu")[5],1),
                        rep(brewer.pal(9,"RdYlBu")[7],1),
                        rep(brewer.pal(9,"RdYlBu")[8],1),
                        rep(brewer.pal(9,"RdYlBu")[9],2)
                 ),
                 col="black")+
  labs(x = "Total Live BA", y = "Count")+
  coord_cartesian(xlim = c(0, 100)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

#CART plot of FIA live BA with inset of histogram
#Saved ("Figures/Lassen-Plumas/FIA_ba_cart.png)
prp(tree_fia, varlen = 0, faclen = 0, type = 3, extra = 1, cex = 0.6,
    box.palette =  c("white", "#f46d43", "#f46d43", "#fdae61"),
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Modern (2011 - 2018) live basal area \n(Live BA ~ max temperature, precipitation, CWD, slope)")
print(h_fia_ba, vp=viewport(.8, .75, .4, .3))
print(grid.text("b", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ))
#dev.off()

####3c. Pine Fraction####

#Historical data with RandomForest
#RF with all 5 parameters
pined <- d[complete.cases(d),c("pine_fraction", model_parms5)]
set.seed(10) #Reproduce bootstrapping 
rfm_pine5 <- randomForest(pine_fraction ~ ., data=pined, importance = TRUE)
print(rfm_pine5)
varImpPlot(rfm_pine5) #Removing aspect

#RF with 4 parameters
pine_parms4 <- names(d[c("tmx_mean8110", "slope", "cwd_mean8110", "ppt_mean8110")])
pined4 <- d[complete.cases(d),c("pine_fraction", pine_parms4)]
set.seed(10) #Reproduce bootstrapping 
rfm_pine4 <- randomForest(pine_fraction ~ ., data=pined4, importance = TRUE)
print(rfm_pine4)
varImpPlot(rfm_pine4) #Removing slope

#RF with 3 parameters
pine_parms3 <- names(d[c("tmx_mean8110", "cwd_mean8110", "ppt_mean8110")])
pined3 <- d[complete.cases(d),c("pine_fraction", pine_parms3)]
set.seed(10) #Reproduce bootstrapping 
rfm_pine3 <- randomForest(pine_fraction ~ ., data=pined3, importance = TRUE)
print(rfm_pine3)
varImpPlot(rfm_pine3) #Removing cwd

#RF with 2 parameters
pine_parms2 <- names(d[c("tmx_mean8110", "ppt_mean8110")])
pined2 <- d[complete.cases(d),c("pine_fraction", pine_parms2)]
set.seed(10) #Reproduce bootstrapping 
rfm_pine2 <- randomForest(pine_fraction ~ ., data=pined2, importance = TRUE)
print(rfm_pine2)
varImpPlot(rfm_pine2) #Removing cwd

#Comparing Random Forests
#Predictions from RandomForest
d$pine5 <- predict(rfm_pine5, d)
d$pine4 <- predict(rfm_pine4, d)
d$pine3 <- predict(rfm_pine3, d)
d$pine2 <- predict(rfm_pine2, d)

#RMSE of BA models
error.summary.pine <- d %>%
  summarise(rmse5 = rmseFun(pine5, pine_fraction),
            rmse4 = rmseFun(pine4, pine_fraction),
            rmse3 = rmseFun(pine3, pine_fraction),
            rmse2 = rmseFun(pine2, pine_fraction))
#Going with pine4 since it has highest %var explained and lowest RMSE

##CART for historicalpine fraction
h.pine <- #Histogram
  ggplot(d)+
  geom_histogram(aes(pine_fraction), binwidth = 0.1,
                 fill=c(rep(brewer.pal(9,"RdYlBu")[1],2),
                        rep(brewer.pal(9,"RdYlBu")[2],2),
                        rep(brewer.pal(9,"RdYlBu")[3],2),
                        rep(brewer.pal(9,"RdYlBu")[5],2),
                        rep(brewer.pal(9,"RdYlBu")[7],2),
                        rep(brewer.pal(9,"RdYlBu")[9],1)
                 ),
                 col="black")+
  labs(x = "Pine fraction", y = "Count")+
  coord_cartesian(xlim = c(0, 1)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

#CART
pine.cart <- rpart(pine_fraction ~ tmx_mean8110 + 
                     ppt_mean8110 +  
                     cwd_mean8110 + 
                     slope, 
                   method = "anova",
                   data = d,
                   control = rpart.control(cp = 0.03))

#Relabeling variables for CART plot
levels(pine.cart$frame$var) <- c("<leaf>", "CWD", "precipitation", "slope", "max temperature")

#CART plot of historical pine fraction
#Saved ("Figures/Lassen-Plumas/QQ_pine_cart.png)
prp(pine.cart, varlen = 0, faclen = 0, type = 3, extra = 1, cex = 0.6, 
    box.palette =  c("#fdae61", "#ffffbf", "#ffffbf"),
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Historical (1924) pine fraction \n(Pine fraction ~ max temperature, precipitation, CWD, slope)")
print(h.pine, vp=viewport(.72, .2, .3, .3))
print(grid.text("a", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ))
#dev.off()

#Applying breakpoints from CART to FIA data
{
  tree_fia <- pine.cart
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$slope < 10 & 
                                d_fia$ppt_mean8110 >= 1497, "pine_fraction"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$slope < 10 & 
                  d_fia$ppt_mean8110 >= 1497, "pine_fraction"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$slope < 10 & 
                                d_fia$ppt_mean8110 < 1497, "pine_fraction"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$slope < 10 & 
                 d_fia$ppt_mean8110 < 1497, "pine_fraction"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$slope >= 10 & 
                                d_fia$tmx_mean8110 >= 15, "pine_fraction"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$slope < 10 & 
                 d_fia$tmx_mean8110 >= 15, "pine_fraction"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$slope >= 10 & 
                                d_fia$tmx_mean8110 < 15 & 
                                d_fia$cwd_mean8110 < 476, "pine_fraction"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$slope < 10 & 
                 d_fia$tmx_mean8110 < 15 & 
                 d_fia$cwd_mean8110 < 476, "pine_fraction"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$slope >= 10 & 
                                d_fia$tmx_mean8110 < 15 & 
                                d_fia$cwd_mean8110 >= 476, "pine_fraction"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$slope < 10 & 
                 d_fia$tmx_mean8110 < 15 & 
                 d_fia$cwd_mean8110 >= 476, "pine_fraction"], na.rm = T)
}

h_fia_pine <- #Histogram
  ggplot(d_fia)+
  geom_histogram(aes(pine_fraction), binwidth = 0.1,
                 fill=c(rep(brewer.pal(9,"RdYlBu")[1],2),
                        rep(brewer.pal(9,"RdYlBu")[2],2),
                        rep(brewer.pal(9,"RdYlBu")[3],2),
                        rep(brewer.pal(9,"RdYlBu")[5],2),
                        rep(brewer.pal(9,"RdYlBu")[7],1),
                        rep(brewer.pal(9,"RdYlBu")[8],1),
                        rep(brewer.pal(9,"RdYlBu")[9],1)
                 ),
                 col="black")+
  labs(x = "Pine fraction", y = "Count")+
  coord_cartesian(xlim = c(0, 1)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

#CART plot of FIA TPH and inset of histogram
#Saved ("Figures/Lassen-Plumas/FIA_pine_cart.png)
prp(tree_fia, varlen = 0, faclen = 0, type = 3, extra = 1, cex = 0.6,
    box.palette =  c("#d73027", "#f46d43", "#fdae61", "#fdae61", "#f46d43", "#f46d43"),
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Modern (2011 - 2018) pine fraction \n(Pine fraction ~ max temperature, precipitation, CWD, slope)")
print(h_fia_pine, vp=viewport(.72, .2, .3, .3))
print(grid.text("b", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ))
#dev.off()

####4. Predictive Maps####

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

  
#Combining raster map values of variables use in RF
s <- stack(tmx,slp,cwd,ppt)
df <- as.data.frame(getValues(s))
df[df<0] <- NA
r <- 1
for(r in 1:nrow(df)){
  if(any(is.na(df[r,]))){
    df[r,] <- NA
  }
}
names_tmp <- names(df)
names(df) <- c("tmx_mean8110", "slope", "cwd_mean8110", "ppt_mean8110")

#Predictions using Random Forest models we chose for CART
df$pred_tph <- predict(rfm_tph4, df)
df$pred_ba <- predict(rfm_ba4, df)
df$pred_pine <- predict(rfm_pine4, df)

#Exclude areas not within the climate envelope of the historical data
df[which(!between(df$tmx_mean8110,range(d$tmx_mean8110)[1],range(d$tmx_mean8110)[2])), ] <-
  NA
df[which(!between(df$slope,range(d$slope)[1],range(d$slope)[2])), ] <-
  NA
df[which(!between(df$cwd_mean8110,range(d$cwd_mean8110)[1],range(d$cwd_mean8110)[2])), ] <-
  NA
df[which(!between(df$ppt_mean8110,range(d$ppt_mean8110)[1],range(d$ppt_mean8110)[2])), ] <-
  NA

#Raster of predictions
#TPH
pred_Align <- cwd
pred_tph <- setValues(x = pred_Align, values = df$pred_tph)
range(getValues(pred_tph), na.rm = T); range(df$pred_tph, na.rm = T)
plot(pred_tph)
writeRaster(pred_tph, "GIS/TPH_Predicted_rfm.tif", overwrite = TRUE)

#Live BA
pred_ba <- setValues(x = pred_Align, values = df$pred_ba)
range(getValues(pred_ba), na.rm = T); range(df$pred_ba, na.rm = T)
plot(pred_ba)
writeRaster(pred_ba, "GIS/BA_Predicted_rfm.tif", overwrite = TRUE)

#Pine fraction
pred_pine <- setValues(x = pred_Align, values = df$pred_pine)
range(getValues(pred_pine), na.rm = T); range(df$pred_pine, na.rm = T)
plot(pred_pine)
writeRaster(pred_pine, "GIS-Pine_Predicted_rfm.tif", overwrite = TRUE)

#Looking at relationship between tph and pine_fraction
tph.pine <- lm(pred_tph ~ pred_pine,
               data = df)

df$pred_tph_pine <- predict(tph.pine, df)

ggplot(df, aes(pred_pine, pred_tph)) + 
  geom_point() + 
  geom_smooth(aes(pred_pine, pred_tph_pine), 
              method = "lm",
              se = FALSE) + 
  theme_bw()+
  labs(x = "Predicted pine fraction", y = "Predicted TPH")+
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9)) +
  geom_text(x = 0.9, y = 65, label = "r^2 = 0.02") +
  geom_text(x=0.9, y=60, label="p < 0.001") +
  ggsave("Figures/Lassen-Plumas/tph_pine.png",
         width = 7, height = 7)

#Creating graphs comparing RF model performance
rf.outputs <- read_xlsx("Data/Derived/rf_performance_metrics.xlsx") %>%
  gather(., "metric", "value", 3:4)

# New facet label for performance metrics
metrics.labs <- c("Variation explained (%)", "RMSE")
names(metrics.labs) <- c("perc.var", "rmse")

ggplot(rf.outputs, aes(num.parameters, value, color = y.var)) + 
  geom_line() + 
  facet_grid(metric~.,
             scales = "free",
             labeller = labeller(metric = metrics.labs)) + 
  theme_bw() + 
  scale_color_discrete(labels = c("Total live BA", "Pine fraction", "TPH")) + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  labs(x = "Number of predictor variables", y = "") + 
  ggsave("Figures/Lassen-Plumas/RF_comparisons.png",
         width = 7, height = 4)

  