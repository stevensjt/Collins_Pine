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

d_summary <- 
  d %>%
  summarise(Tot_TPA = mean(Tot_TPA, na.rm = T),
            Tot_TPH = mean(Tot_TPH, na.rm = T),
            BA_Tot = mean(TotLvBA_ac),
            BAmet_Tot = mean(TotLvBA_met),
            Mean_pine_fraction = mean(pine_fraction),
            Mean_elev = mean(elev_mean),
            Min_elev = round(min(elev_mean),0),
            Max_elev = round(max(elev_mean),0),
            Mean_precip = mean(ppt_mean8110),
            Mean_temp = mean(tmx_mean8110)
  )

d <- #Remove 30 rows that are missing climate data because they're under Lake Almanor
  d[-which(!complete.cases(d)),]

####3. Analysis####
#Checking correlations amongst variables to see if we can eliminate some
library(PerformanceAnalytics)
correlation.data <- d %>%
  select(21:30,
         -aspect)
chart.Correlation(correlation.data, histogram = TRUE, pch = 19)
#Just going to keep elevation, slope, aspect, mean cwd, and precipitation
model_parms <- names(d[c("elev_mean", "slope", "aspect", "cwd_mean8110", "ppt_mean8110")])

###3a. All trees, density####

#Historical data with RandomForest
library(randomForest)
md <- d[complete.cases(d),c("Tot_TPH", model_parms)]
rfm_tph <- randomForest(Tot_TPH ~ ., data=md, importance = TRUE)
print(rfm_tph)
varImpPlot(rfm_tph) #Variable importance plot

#Checking model parameters to see if default settings are okay (# trees and # vars at each node)
plot(rfm_tph[[5]]) #Looking to see if default number of trees is sufficient to explain variation in data; default is good
rsq.tph <- vector(length = 5) #Checking to see if number of variables for each node is okay; default is good
for (i in 1:5) {
  temp.model <- randomForest(Tot_TPH ~ ., data=md, mtry = i, importance = TRUE)
  rsq.tph[i] <- temp.model[[5]][500]
  }
plot(rsq.tph) #Default # of variables at each node looks good

#Historical data with GLM
#make a function to standardize continuous variables
standFun <- function(x) 
{stnd <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}

#Standardizing continous variables
d <- d%>%
  mutate(s.elev = standFun(elev_mean),
         s.slope = standFun(slope),
         s.cwd = standFun(cwd_mean8110),
         s.ppt = standFun(ppt_mean8110))

#Indicating which standardized variables to include in GLM
s_parms <- names(d[c("s.elev", "s.slope", "s.cwd", "s.ppt", "aspect")])

#Gmulti for historical TPH
m_tph <- glmulti(y="Tot_TPH", 
                 xr=s_parms,
                 data=d,
                 level=1,method="h", plotty = FALSE)

#TPH model results
lapply(m_tph@objects, function(x) AIC(x)) #First two models within 8 AIC
lapply(1:2, function(x) summary(m_tph@objects[[x]])) #Going with m_tph@objects[[2]]; less variables
par(mfrow=c(2,2))
plot(m_tph@objects[[2]]) #Diagnostic plots; looks acceptable
dev.off()
r.squaredGLMM(m_tph@objects[[2]]) #8% var explained; not great

#Comparing Random Forest to GLM
#Predictions from RandomForest
d$rf_tph <- predict(rfm_tph, d)
d$glm_tph <- predict(m_tph@objects[[2]])

#Funciton to estimate RMSE
rmseFun <- function(a, b)
{rmse <- sqrt(mean((a - b)^2))}

#RMSE of TPH models
error.summary.tph <- d %>%
  summarise(rmse.rf = rmseFun(rf_tph, Tot_TPH),
            rmse.glm = rmseFun(glm_tph, Tot_TPH)) #RMSE is less in Random Forest

#Check predictive power; okay, but not great for either model
#Based on RMSE and % variance explained, going with Random Forest
par(mfrow=c(1,2))
plot(d$Tot_TPH ~ d$rf_tph)
abline(0,1)
plot(d$Tot_TPH ~ d$glm_tph)
abline(0,1)
dev.off()

##CART for historical tph
h.tph <- #Histogram
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
  labs(x = "TPH", y = "Count")+
  coord_cartesian(xlim = c(0, 60)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

#CART
tph.cart <- rpart(Tot_TPH ~ elev_mean + 
                slope +
                cwd_mean8110 + 
              ppt_mean8110 + 
              aspect, 
        method = "anova",
        data = d,
        control = rpart.control(cp = 0.02))

#Relabeling variables for CART plot
levels(tph.cart$frame$var) <- c("<leaf>", "CWD", "elevation", "precipitation")
  
#CART plot of historical TPH and inset of tph histogram
#Saved ("Figures/Lassen-Plumas/QQ_tph_cart.png)
prp(tph.cart, varlen = 0, faclen = 0, type = 3, extra = 1, cex = 0.6, 
    box.palette =  c("#abd9e9", "#74add1", "#4575b4"),
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Historical (1924) tree density \n(TPH ~ elevation, slope, aspect, CWD, precipitation)")
print(h.tph, vp=viewport(.24, .18, .4, .3))
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

#Applying breakpoints from CART to FIA data
{
  tree_fia <- tph.cart
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$elev_mean >= 1702, "Tot_TPH"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$elev_mean >= 1792, "Tot_TPH"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$elev_mean < 1702 &
                                d_fia$ppt_mean8110 < 1020, "Tot_TPH"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$elev_mean < 1702 &
                 d_fia$ppt_mean8110 < 1020, "Tot_TPH"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$elev_mean < 1702 &
                                d_fia$ppt_mean8110 >= 1020 & 
                                d_fia$cwd_mean8110 < 517, "Tot_TPH"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$elev_mean < 1702 &
                 d_fia$ppt_mean8110 >= 1020 & 
                 d_fia$cwd_mean8110 < 517, "Tot_TPH"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$elev_mean < 1702 &
                                d_fia$ppt_mean8110 >= 1020 & 
                                d_fia$cwd_mean8110 >= 517, "Tot_TPH"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$elev_mean < 1702 &
                 d_fia$ppt_mean8110 >= 1020 & 
                 d_fia$cwd_mean8110 >= 517, "Tot_TPH"], na.rm = T)

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
    box.palette =  c("#f46d43", "#fdae61"),
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Modern (2011 - 2018) tree density \n(TPH ~ elevation, slope, aspect, CWD, precipitation)")
print(h_fia_tph, vp=viewport(.24, .18, .4, .3))
print(grid.text("b", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ) )
#dev.off()

####3b. Basal Area####

#Historical data with RandomForest
bad <- d[complete.cases(d),c("TotLvBA_met", model_parms)]
rfm_ba <- randomForest(TotLvBA_met ~ ., data=bad, importance = TRUE)
print(rfm_ba)
varImpPlot(rfm_ba) #Variable importance plot
plot(rfm_ba[[5]]) #Looking to see if default number of trees is sufficient to explain variation in data; default is good
rsq.ba <- vector(length = 5) #Checking to see if number of variables for each node is okay; default is good
for (i in 1:5) {
  temp.model <- randomForest(TotLvBA_met ~ ., data=bad, mtry = i, importance = TRUE)
  rsq.ba[i] <- temp.model[[5]][500]
}
plot(rsq.ba)

#Gmulti for historical Live BA
m_ba <- glmulti(y="TotLvBA_met", 
                 xr=s_parms,
                 data=d,
                 level=1,method="h", plotty = FALSE)

#Live BA model results
lapply(m_ba@objects, function(x) AIC(x)) #First four models within 8 AIC
lapply(1:4, function(x) summary(m_ba@objects[[x]])) #Going with m_ba@objects[[1]]; less variables
par(mfrow=c(2,2))
plot(m_ba@objects[[1]]) #Diagnostic plots; looks acceptable
dev.off()
r.squaredGLMM(m_ba@objects[[1]]) #6% var explained; not great

#Comparing Random Forest to GLM
#Predictions from RandomForest
d$rf_ba <- predict(rfm_ba, d)
d$glm_ba <- predict(m_ba@objects[[1]], d)

#RMSE of Live BA models
error.summary.ba <- d %>%
  summarise(rmse.rf = rmseFun(rf_ba, TotLvBA_met),
            rmse.glm = rmseFun(glm_ba, TotLvBA_met)) #RMSE is less in Random Forest

#Check predictive power; okay, but not great for either model
#Based on RMSE and % variance explained, going with Random Forest
par(mfrow=c(1,2))
plot(d$TotLvBA_met ~ d$rf_ba)
abline(0,1)
plot(d$TotLvBA_met ~ d$glm_ba)
abline(0,1)
dev.off()

##CART for historical Live BA
h.ba <- #Histogram
  ggplot(d)+
  geom_histogram(aes(TotLvBA_met), binwidth = 5, center = 2.5,
                 fill=c(rep(brewer.pal(9,"RdYlBu")[1],2),
                        rep(brewer.pal(9,"RdYlBu")[2],1),
                        rep(brewer.pal(9,"RdYlBu")[3],1),
                        rep(brewer.pal(9,"RdYlBu")[5],1),
                        rep(brewer.pal(9,"RdYlBu")[7],1),
                        rep(brewer.pal(9,"RdYlBu")[9],3)
                 ),
                 col="black")+
  labs(x = "Total Live BA", y = "Count")+
  coord_cartesian(xlim = c(0, 50)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

#CART
ba.cart <- rpart(TotLvBA_met ~ elev_mean + 
                    slope +
                    cwd_mean8110 + 
                    ppt_mean8110 + 
                    aspect, 
                  method = "anova",
                  data = d,
                  control = rpart.control(cp = 0.02))

#Relabeling variables for CART plot
levels(ba.cart$frame$var) <- c("<leaf>", "CWD", "elevation", "precipitation")

#CART plot of historical live BA
#Saved ("Figures/Lassen-Plumas/QQ_ba_cart.png)
prp(ba.cart, varlen = 0, faclen = 0, type = 3, extra = 1, cex = 0.6, 
    box.palette =  c("#f46d43", "#fdae61", "#fdae61"),
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Historical (1924) live basal area \n(Live BA ~ elevation, slope, aspect, CWD, precipitation)")
print(h.ba, vp=viewport(.74, .70, .4, .3))
print(grid.text("a", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ))#dev.off()

#Applying breakpoints from CART to FIA data
{
  tree_fia <- ba.cart
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$elev_mean >= 1661, "TotLvBA_met"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$elev_mean >= 1661, "TotLvBA_met"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$elev_mean < 1661 &
                                d_fia$cwd_mean8110 < 434, "TotLvBA_met"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$elev_mean < 1661 &
                 d_fia$cwd_mean8110 < 434, "TotLvBA_met"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$elev_mean < 1661 &
                                d_fia$cwd_mean8110 >= 434 & 
                                d_fia$ppt_mean8110 < 1015 & 
                                d_fia$elev_mean >= 1473, "TotLvBA_met"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$elev_mean < 1661 &
                 d_fia$cwd_mean8110 >= 434 & 
                 d_fia$ppt_mean8110 < 1015 & 
                 d_fia$elev_mean >= 1473, "TotLvBA_met"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$elev_mean < 1661 &
                                d_fia$cwd_mean8110 >= 434 & 
                                d_fia$ppt_mean8110 < 1015 & 
                                d_fia$elev_mean < 1473, "TotLvBA_met"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$elev_mean < 1661 &
                 d_fia$cwd_mean8110 >= 434 & 
                 d_fia$ppt_mean8110 < 1015 & 
                 d_fia$elev_mean < 1473, "TotLvBA_met"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$elev_mean < 1661 &
                                d_fia$cwd_mean8110 >= 434 & 
                                d_fia$ppt_mean8110 >= 1015 & 
                                d_fia$ppt_mean8110 >= 1174, "TotLvBA_met"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$elev_mean < 1661 &
                 d_fia$cwd_mean8110 >= 434 & 
                 d_fia$ppt_mean8110 >= 1015 & 
                 d_fia$ppt_mean8110 >= 1174, "TotLvBA_met"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[6], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$elev_mean < 1661 &
                                d_fia$cwd_mean8110 >= 434 & 
                                d_fia$ppt_mean8110 >= 1015 & 
                                d_fia$ppt_mean8110 < 1174, "TotLvBA_met"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[6], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$elev_mean < 1661 &
                 d_fia$cwd_mean8110 >= 434 & 
                 d_fia$ppt_mean8110 >= 1015 & 
                 d_fia$ppt_mean8110 < 1174, "TotLvBA_met"], na.rm = T)
  
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
    box.palette =  c("white", "#d73027", "#d73027", "#fdae61"),
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Modern (2011 - 2018) live basal area \n(Live BA ~ elevation, slope, aspect, CWD, precipitation)")
print(h_fia_ba, vp=viewport(.74, .70, .4, .3))
print(grid.text("b", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ))
#dev.off()

####3c. Pine Fraction####

#Historical data with RandomForest
pined <- d[complete.cases(d),c("pine_fraction", model_parms)]
rfm_pine <- randomForest(pine_fraction ~ ., data=pined, importance = TRUE)
print(rfm_pine)
varImpPlot(rfm_pine) #Variable importance plot
plot(rfm_pine[[5]]) #Looking to see if default number of trees is sufficient to explain variation in data; default is good
rsq.pine <- vector(length = 5) #Checking to see if number of variables for each node is okay; default is good
for (i in 1:5) {
  temp.model <- randomForest(pine_fraction ~ ., data=pined, mtry = i, importance = TRUE)
  rsq.pine[i] <- temp.model[[5]][500]
}
plot(rsq.pine)

#Gmulti for historical pine fraction
m_pine <- glmulti(y="pine_fraction", 
                xr=s_parms,
                data=d,
                level=1,method="h", plotty = FALSE)

#Pine fraction model results
lapply(m_pine@objects, function(x) AIC(x)) #First two models within 8 AIC
lapply(1:2, function(x) summary(m_pine@objects[[x]])) #Going with m_pine@objects[[1]]
par(mfrow=c(2,2))
plot(m_pine@objects[[1]]) #Diagnostic plots; looks acceptable
dev.off()
r.squaredGLMM(m_pine@objects[[1]]) #22% var explained; better than other metrics

#Comparing Random Forest to GLM
#Predictions from RandomForest
d$rf_pine <- predict(rfm_pine, d)
d$glm_pine <- predict(m_pine@objects[[1]], d)

#RMSE of pine fraction models
error.summary.pine <- d %>%
  summarise(rmse.rf = rmseFun(rf_pine, pine_fraction),
            rmse.glm = rmseFun(glm_pine, pine_fraction)) #RMSE is less in Random Forest

#Check predictive power; okay, but not great for either model
#Based on RMSE and % variance explained, going with Random Forest
par(mfrow=c(1,2))
plot(d$pine_fraction ~ d$rf_pine)
abline(0,1)
plot(d$pine_fraction ~ d$glm_pine)
abline(0,1)
dev.off()

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
pine.cart <- rpart(pine_fraction ~ elev_mean + 
                   slope +
                   cwd_mean8110 + 
                   ppt_mean8110 + 
                   aspect, 
                 method = "anova",
                 data = d,
                 control = rpart.control(cp = 0.02))

#Relabeling variables for CART plot
levels(pine.cart$frame$var) <- c("<leaf>", "CWD", "elevation", "precipitation", "slope")

#CART plot of historical pine fraction
#Saved ("Figures/Lassen-Plumas/QQ_pine_cart.png)
prp(pine.cart, varlen = 0, faclen = 0, type = 3, extra = 1, cex = 0.6, 
    box.palette =  c("#fdae61", "#ffffbf", "#ffffbf"),
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Historical (1924) pine fraction \n(Pine fraction ~ elevation, slope, aspect, CWD, precipitation)")
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
                                d_fia$elev_mean < 1558, "pine_fraction"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$slope >= 10 & 
                 d_fia$elev_mean < 1558, "pine_fraction"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$slope >= 10 & 
                                d_fia$elev_mean >= 1558 & 
                                d_fia$cwd_mean8110 < 491, "pine_fraction"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$slope >= 10 & 
                 d_fia$elev_mean >= 1558 & 
                 d_fia$cwd_mean8110 < 491, "pine_fraction"], na.rm = T)
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$slope >= 10 & 
                                d_fia$elev_mean >= 1558 & 
                                d_fia$cwd_mean8110 >= 491, "pine_fraction"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "yval" ] <- #Get modern mean
    mean(d_fia[d_fia$slope >= 10 & 
                 d_fia$elev_mean >= 1558 & 
                 d_fia$cwd_mean8110 >= 491, "pine_fraction"], na.rm = T)
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
    box.palette =  c("#f46d43", "#fdae61", "#f46d43"),
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Modern (2011 - 2018) pine fraction \n(Pine fraction ~ elevation, slope, aspect, CWD, precipitation)")
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

  
#Combining raster map values
s <- stack(dem,slp,asp,cwd,ppt)
df <- as.data.frame(getValues(s))
df[df<0] <- NA
r <- 1
for(r in 1:nrow(df)){
  if(any(is.na(df[r,]))){
    df[r,] <- NA
  }
}
names_tmp <- names(df)
names(df) <- c("elev_mean", "slope", "aspect", "cwd_mean8110", "ppt_mean8110")
df$aspect_cat <- ifelse(between(df$aspect, 135,315),"SW","NE")
df$aspect <- factor(df$aspect_cat)

#Predictions using Random Forest
df$pred_tph <- predict(rfm_tph, df)
df$pred_ba <- predict(rfm_ba, df)
df$pred_pine <- predict(rfm_pine, df)

#Exclude areas not within the climate envelope of the historical data
df[which(!between(df$elev_mean,range(d$elev_mean)[1],range(d$elev_mean)[2])), ] <-
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
writeRaster(pred_pine, "GIS/Pine_Predicted_rfm.tif", overwrite = TRUE)


  