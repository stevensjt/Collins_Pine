
####0. Read libraries####
library(readxl) #For read_excel() and others; version 1.0.0
library(tidyverse) #For ggplot() and others; version 1.2.1
library(tableHTML) #For write_tableHTML(); version 1.1.0
library(glmulti) #For glmulti(); version 1.0.7
library(rpart) #For rpart(); version 4.1-11
library(rpart.plot) #for prp(); version 2.1.2
library(RColorBrewer) #for brewer.pal(); version 1.1-2
library(grid) #for viewport(); version 3.4.3
library(plotrix) #for std.error(); version 37
library(cowplot) #for plot_grid(); version 0.9.2
library(raster) #for raster(); version 3.4.3

####1. Data assembly and summary tables####
###1a. Timber inventory data assembly
##Read in data
dens <- #Read in density data
  read_excel("./Data/Raw/EldoTreeData.xlsx",sheet="density")
names(dens)[names(dens)=="LotCode"] <- "id" #"id" is the unique identifier for a qq section
ba <- #Read in basal area data
  read_excel("./Data/Raw/EldoTreeData.xlsx",sheet="BA")
names(ba)[names(ba)=="Lotcode"] <- "id"
d <- #Master data frame for timber inventory analysis; merge density and basal area data frames
  merge.data.frame(dens[,-2], ba[,-c(2:3)], by = "id")

##Aggregate data rows to the qq section (a few select cases)
i <- #Identify qq sections where data are separated into two transects per section (i.e. N and S); 
  #N = 22 here, but there are 12 unique plots (10 duplicated plus 2 with either only N or only S)
  sort(c(grep("E",d$id),grep("W",d$id),grep("N",d$id),grep("S",d$id)))
d$id[i] <- #Remove N/S modifiers from id field
  substr(d$id[i],1,nchar(d$id[i])-1) 
i2 <- #Identify the first of every duplicated pair; N = 10
  which(duplicated(d$id)|duplicated(d$id,fromLast = TRUE))[c(TRUE, FALSE)]
d[i2,c(3:20, 22:ncol(d))] <- #Average the two duplicated rows together (TPH and BA values; equal weights)
  (d[i2,c(3:20, 22:ncol(d))] + d[i2+1,c(3:20, 22:ncol(d))]) / 2
d <- d[-(i2+1),] #Remove the second duplicated row; remaining N = 642

##Add additional variables
d$AB_PI <- #Calculate fir:pine ratio
  (d$ABCO_met+d$ABMA_met) / 
  (d$ABCO_met + d$ABMA_met + d$PICO_met+d$PIJE_met+d$PILA_met+d$PIMO_met+d$PIPO_met)
d$SampleYear <- #Add variable for year sampled; Twnshp 10 (southern) from 1923 and T13 (northern) from 1936
  ifelse(substr(d$id,1,2) == "10", "1923", "1936")
d <- #Add in climate data
  #eldo_extractions also has N = 642; this file was creaed in SpatialExtractions.R
  merge.data.frame(d, read_csv("./Data/Derived/eldo_extractions.csv", col_types = cols()), by = "id") 

##Tidy data; metadata
d[is.na(d$D6_eng),c("TPH0","TPH1","TPH2","TPH3","TPH4","TPH_Tot")] <- 
  NA #16 plots from T10 sectn 6 are missing density observations, set zero values from zero to NA.
d_vars_to_keep <- #Identify variables of interest
  c("id", "SampleYear",  "D6_met","D12_met","D18_met","D24_met","D30_met","TPH1", "TPH2", "TPH3", "TPH4", "TPH_Tot", 
    "ABCO_met", "ABMA_met", "CADE_met", "JUNIPER_met", "PICO_met", "PIJE_met", "PILA_met", 
    "PIMO_met", "PIPO_met", "PSME_met", "QUCH_met", "QUKE_met", "TotBA_met", "AB_PI", 
    "elev_mean", "cwd_mean8110", "cwd_jun_mean8110", "aet_mean8110", "spk_mean8110", "ppt_mean8110", 
    "tmx_mean8110", "slope", "aspect_cat")
#d_metadata <- c("Lotcode id (qq section)", "year sampled", "trees per ha (tph) 15.2-30.5 cm", "tph 30.5-61.0 cm", "tph 61.0-91.4 cm", "tph >91.4 cm", "TPH_Tot = total tph >15.2 cm, from tallying individual size classes (accurate)", "TPH0 = Likely density of trees <15.2 cm; not tallied on all transects", "Basal Area (BA) in m2/ha for ABCO", "BA in m2/ha for ABMA", "BA in m2/ha for CADE", "BA in m2/ha for JUNIPER spp", "BA in m2/ha for PICO", "BA in m2/ha for PIJE", "BA in m2/ha for PILA", "BA in m2/ha for PIMO", "BA in m2/ha for PIPO", "BA in m2/ha for PSME", "BA in m2/ha for QUCH","BA in m2/ha for QUKE", "Total basal area in m2/ha", "ABIES:PINUS ratio","mean elevation in m", "mean climatic water deficit 1981-2010", "mean June CWD", "mean actual evapotranspiraton", "mean April 1 snowpack", "mean annual precipitation", "mean high temperature",  "mean slope (averaged within QQ section; degrees)" ,"aspect (at centroid of QQ section; SW if between 135 and 315 degrees, otherwise NE)")
d <- #Only keep variables of interest (can update later if needed)
  d[,pmatch(d_vars_to_keep,names(d))]
d <- #Only keep rows where BA > 9 (this matches FIA filter which captures "nonstocked" plots)
  #This filter removes three plots that lacked any trees, 
  #This filter removes 16 plots that had some trees but very low BA (mean = 5.3 m2/ha)
  d[-which(d$TotBA_met < 9),] 
names(d)[which(names(d) == "aspect_cat")] <- "aspect" #have to do this for regression tree
#Final N = 623
#Climatic and topographic means as per reviewer comment
mns <- data.frame(
  mean = colMeans(d[,c(27:34)]),
  sd = apply(d[,c(27:34)],2,sd),
  min = apply(d[,c(27:34)],2,min),
  max = apply(d[,c(27:34)],2,max)
)
mns <- round(mns, 2)

###1b. FIA data assembly
##Read in data
d_fia <- #Read in FIA data; N = 108
  #Includes plots between 1230 and 2438 m, live BA > 9 m2/ha (see Eldo_FIA_processing for details)
  read_csv("./Data/Derived/eldo_FIA_processed.csv", col_types = cols())
names(d_fia)[1] <- "id"
d_fia$id <- as.character(d_fia$id)
d_fia <- #Add in climate data
  #eldo_extractions also has N = 108; this file was creaed in SpatialExtractions.R
  merge.data.frame(d_fia, read_csv("./Data/Derived/eldo_FIA_extractions.csv", col_types = cols()), by = "id")

##Add additional variables
d_fia$AB_PI <- #Calculate fir:pine ratio
  (d_fia$ABCO_met+d_fia$ABMA_met) / 
  (d_fia$ABCO_met + d_fia$ABMA_met + d_fia$PICO_met+d_fia$PIJE_met+d_fia$PILA_met+d_fia$PIMO_met+d_fia$PIPO_met)
d_fia$elev_diff <- d_fia$ELEV_met - d_fia$elev_extract

##Filter data
d_fia_vars_to_keep <- #Identify variables of interest
  c("id","INVYR","LAT","LON","ELEV_met","elev_extract", "elev_diff", 
    "STDAGE_cond","FORTYPCD_cond", "OWNGRPCD_cond",  "DSTRBCD_cond", "CONDPROP_cond", 
    "DENS_6_12", "DENS_12_24", "DENS_24_36", "DENS_36plus", "DENS_6plus", 
    "ABCO_met", "ABMA_met", "CADE_met", "JUNIPER_met", "PICO_met", "PIJE_met", "PILA_met", "PIMO_met", 
    "PIPO_met", "PSME_met", "QUCH_met", "QUKE_met", "BALIVE_sqmha", "BALIVE_sqmha_check", "AB_PI", 
    "cwd_mean8110", "cwd_jun_mean8110", "aet_mean8110", "spk_mean8110", "ppt_mean8110", "tmx_mean8110", 
    "slope", "aspect_cat")
d_fia <- #Only keep variables of interest (can update later if needed)
  d_fia[,pmatch(d_fia_vars_to_keep,names(d_fia))]
names(d_fia) <- #Assign proper names to variables of interest (ideally matching "d")
  c("id","INVYR","LAT","LON","elev_mean","elev_extract", "elev_diff", 
    "STDAGE_cond","FORTYPCD_cond", "OWNGRPCD_cond", "DSTRBCD_cond", "CONDPROP_cond", 
    "TPH1", "TPH2", "TPH3", "TPH4", "TPH_Tot", 
    "ABCO_met", "ABMA_met", "CADE_met", "JUNIPER_met", "PICO_met", "PIJE_met", "PILA_met", "PIMO_met", 
    "PIPO_met", "PSME_met", "QUCH_met", "QUKE_met", "TotBA_met", "BALIVE_sqmha_check", "AB_PI", 
    "cwd_mean8110", "cwd_jun_mean8110", "aet_mean8110", "spk_mean8110", "ppt_mean8110", "tmx_mean8110", 
    "slope", "aspect_cat")
names(d_fia)[which(names(d_fia) == "aspect_cat")] <- "aspect" #have to do this for regression tree
d_fia <- #Remove plots inventoried in 1994; these plots
  #1) were all fuzzed 1 mile (FIA user guide) vs 0.5 miles for more recent plots,
  #2) were all on private land (so there may be correlated bias between ownership and sampling year)
  #3) Contain a majority of the plots with >150 m elevation difference between FIA and extracted estimates
  #(Of the 9 plots with >150m difference, 6 are from the 1994 survey)
#  d_fia[-which(d_fia$INVYR == 1994),] #Already took care of 1994 plots in Eldo_FIA_processing.R
d_fia <- #Remove plots with >150 m elevation difference between FIA and extractions (N = 3)
  d_fia[-which(abs(d_fia$elev_diff) > 150),]
row.names(d_fia) <- #Rename rows to account for missing row numbers from deletions
  1:nrow(d_fia)
#length(grep(40,d_fia$OWNGRPCD_cond))/nrow(d_fia) #20% of remaining 91 plots are at least partially on private land.
#Final N = 91

###1c. Timber inventory - FIA comparison summary table
d_summary <- 
  d %>%
  group_by(SampleYear) %>%
  summarise(TPH1 = mean(TPH1, na.rm = T),
            TPH2 = mean(TPH2, na.rm = T),
            TPH3 = mean(TPH3, na.rm = T),
            TPH4 = mean(TPH4, na.rm = T),
            TPH_Tot = mean(TPH_Tot, na.rm = T),
            BA_Tot = mean(TotBA_met),
            Mean_AB_PI = mean(AB_PI, na.rm = T),
            Mean_elev = mean(elev_mean),
            Mean_precip = mean(ppt_mean8110),
            Mean_temp = mean(tmx_mean8110)
  )

d_fia_summary <-
  d_fia %>%
  summarise(TPH1 = mean(TPH1, na.rm = T),
            TPH2 = mean(TPH2, na.rm = T),
            TPH3 = mean(TPH3, na.rm = T),
            TPH4 = mean(TPH4, na.rm = T),
            TPH_Tot = mean(TPH_Tot, na.rm = T),
            BA_Tot = mean(TotBA_met),
            Mean_AB_PI = mean(AB_PI, na.rm = T),
            Mean_elev = mean(elev_mean),
            Mean_precip = mean(ppt_mean8110),
            Mean_temp = mean(tmx_mean8110)
  )

#d_fia_unique_yrs <- d_fia %>% group_by(INVYR) %>% tally()
#Even coverage across sampled years, range from 2001-2010 and n=6 to n=14)  

t1 <- round(rbind(d_summary[,-1],d_fia_summary),2)
t1 <- round(as.data.frame(t(t1)),2)
t1 <- as.data.frame(apply(t1,2,as.character), row.names = rownames(t1), stringsAsFactors = FALSE)
names(t1) <- c("1923", "1936", "2001-2010")
rownames(t1) <- c("TPH \n15-30", "TPH \n30-60", "TPH \n 60-91", "TPH \n>91", "Total TPH",
                  "Total BA", "Mean Abies:Pinus \nratio", "Mean \nelevation (m)", 
                  "Mean annual \nprecipitation (mm)", "Mean annual \ntemperature (C)")
#write_tableHTML(tableHTML(t1), file = './Tables/Table 1.html')

###1d. Clean up working environment
rm(d_fia_vars_to_keep,d_vars_to_keep,i,i2,dens,ba)

####2. Exploratory data analysis####

###2a. Determine sampling year effects
##Comparison models
summary(#Year has significant positive effect, 44 more tph in 1936 than 1923
  glm(formula = TPH_Tot ~ SampleYear, data = d))
summary(#Year has no significant effect on basal area.
  glm(formula = TotBA_met ~ SampleYear, data = d))
summary(#Year has significant positive effect on 6-12 inch trees, 43 more tph in 1936 than 1923 
  glm(formula = D6_met ~ SampleYear, data = d))
summary(#Year has significant positive effect on 12-18 inch trees, 9 more tph in 1936 than 1923
  glm(formula = D12_met ~ SampleYear, data = d))
summary(#Year has no significanteffect on 18-24 inch trees, 0.5 fewer tph in 1936 than 1923
  glm(formula = D18_met ~ SampleYear, data = d))
summary(#Year has a significant negative effect on 24-30 inch trees, 1.5 fewer tph in 1936 than 1923
  glm(formula = D24_met ~ SampleYear, data = d))
summary( #Year has no significant negative effect on 30-36 inch trees, 2.5 fewer tph in 1936 than 1923
  glm(formula = D30_met ~ SampleYear, data = d))
summary(
  #After accounting for environment, year does not have significant effect for all trees, 
  #but has a significant positive effect for small trees (only in the smallest size class, D6_met)
  glm(formula = D6_met ~ 1 + cwd_mean8110 + aet_mean8110 + spk_mean8110 + 
        ppt_mean8110 + tmx_mean8110 + slope + aspect + SampleYear, data = d)
  )

##Calculate correction factors
d_q <- d[,c(2:7)] %>%
  #Generate data frame with density for five smallest size classes, by year
  gather(key = size_class, value = tph, D6_met:D30_met, factor_key = TRUE) %>%
  group_by(SampleYear, size_class) %>%
  summarise(mean_tph = mean(tph, na.rm = T),stderr_tph = std.error(tph, na.rm = T)) %>%
  as.data.frame()

cor36_23_d6 <- 
  d_q[d_q$SampleYear=="1923","mean_tph"][1]/d_q[d_q$SampleYear=="1936","mean_tph"][1]
cor36_23_d12 <- 
  d_q[d_q$SampleYear=="1923","mean_tph"][2]/d_q[d_q$SampleYear=="1936","mean_tph"][2]

d$TPH1_orig <- d$TPH1
d$D6_met_orig <- d$D6_met
d$TPH2_orig <- d$TPH2
d$D12_met_orig <- d$D12_met
d$D6_met[d$SampleYear == "1936"] <- d$D6_met[d$SampleYear == "1936"] * cor36_23_d6
d$D12_met[d$SampleYear == "1936"] <- d$D12_met[d$SampleYear == "1936"] * cor36_23_d12

d_q_cor <- d[,c(2:7)] %>%
  #Generate data frame with density for five smallest size classes, by year, with corrected values
  gather(key = size_class, value = tph, D6_met:D30_met, factor_key = TRUE) %>%
  group_by(SampleYear, size_class) %>%
  summarise(mean_tph = mean(tph, na.rm = T),stderr_tph = std.error(tph, na.rm = T)) %>%
  as.data.frame()

##Make figure A1
levels(d_q$size_class) <- c("15-30", "30-46", "46-61", "61-76", "76-91")
levels(d_q_cor$size_class) <- c("15-30", "30-46", "46-61", "61-76", "76-91")

FigA1a <-
ggplot(d_q, aes(x = size_class, y = mean_tph, fill = SampleYear)) + 
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = mean_tph-stderr_tph, ymax = mean_tph+stderr_tph), 
                position = position_dodge(width=0.9), width = 0.25) + 
  scale_fill_manual(values = c("gray75","gray25")) + 
  labs(x = "Size class (cm)", y = bquote('Mean trees '*ha^-1* ' '), 
       fill = "Sample year") + 
  theme_bw() + 
  theme(legend.position = c(0.8,0.6))

FigA1b <- 
  ggplot(d_q_cor, aes(x = size_class, y = mean_tph, fill = SampleYear)) + 
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = mean_tph-stderr_tph, ymax = mean_tph+stderr_tph), 
                position = position_dodge(width=0.9), width = 0.25) + 
  scale_fill_manual(values = c("gray75","gray25")) + 
  labs(x = "Size class (cm)", y = bquote('Mean trees '*ha^-1* ' '), 
       fill = "Sample year") + 
  theme_bw() + 
  theme(legend.position = "none")


pdf("./Figures/El Dorado/FigA1_v3.pdf")
plot_grid(FigA1a,FigA1b, ncol = 1, labels = c("a", "b"))
dev.off()

###2b. Updated summary table
d_summary <- #Update historical summary table to account for tph correction in small trees.
  d %>%
  summarise(TPH1 = mean(TPH1, na.rm = T),
            TPH2 = mean(TPH2, na.rm = T),
            TPH3 = mean(TPH3, na.rm = T),
            TPH4 = mean(TPH4, na.rm = T),
            TPH_Tot = mean(TPH_Tot, na.rm = T),
            BA_Tot = mean(TotBA_met),
            Mean_AB_PI = mean(AB_PI, na.rm = T),
            Mean_elev = mean(elev_mean),
            Mean_precip = mean(ppt_mean8110),
            Mean_temp = mean(tmx_mean8110)
  )

t1 <- round(rbind(d_summary,d_fia_summary),2)
t1 <- round(as.data.frame(t(t1)),2)
t1 <- as.data.frame(apply(t1,2,as.character), row.names = rownames(t1), stringsAsFactors = FALSE)
names(t1) <- c("1923", "2001-2010")
rownames(t1) <- c("TPH \n15-30", "TPH \n30-60", "TPH \n 60-91", "TPH \n>91", "Total TPH",
                  "Total BA", "Mean Abies:Pinus \nratio", "Mean \nelevation (m)", 
                  "Mean annual \nprecipitation (mm)", "Mean annual \ntemperature (C)")
#write_tableHTML(tableHTML(t1), file = './Tables/Table 1 corrected.html')


###2c. Clean up working environment
rm(d_q, d_q_cor, cor36_23_d12, cor36_23_d6, d_fia_summary, d_summary)

####3. Model selection and regression trees####
vfirst <- which(names(d)=="elev_mean")
vlast <- which(names(d)=="aspect")
potential_parms <- names(d[c(vfirst:vlast)])
d$TPH_large <- d$TPH3 + d$TPH4
d_fia$TPH_large <- d_fia$TPH3 + d_fia$TPH4

###3a. All trees
{
##historical data
m_tph <- glmulti(y="TPH_Tot", 
             xr=potential_parms,
             data=d,
             level=1,method="h", plotty = FALSE) 
summary(m_tph@objects[[1]])
r.squaredGLMM(m_tph@objects[[1]])

tree <- #Best model according to glmulti. Using this one to create regression tree 
  rpart(m_tph@objects[[1]]$formula, 
        method = "anova",
        data = d,
        control = rpart.control(cp = 0.02))
levels(tree$frame$var) <- c("<leaf>", "deficit", "precip", "snowpack")

h <- #Histogram
  ggplot(d)+
  geom_histogram(aes(TPH_Tot), binwidth = 30, center = 15,
                 fill=c(rep(brewer.pal(9,"RdYlBu")[1],2),
                        rep(brewer.pal(9,"RdYlBu")[2],1),
                        rep(brewer.pal(9,"RdYlBu")[3],1),
                        rep(brewer.pal(9,"RdYlBu")[5],1),
                        rep(brewer.pal(9,"RdYlBu")[7],1),
                        rep(brewer.pal(9,"RdYlBu")[8],4),
                        rep(brewer.pal(9,"RdYlBu")[9],6)
                        ),
                 col="black")+
  labs(x = "TPH", y = "Count")+
  coord_cartesian(xlim = c(0, 800)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

cairo_pdf("./Figures/El Dorado/Fig2a_v3.pdf", height = 3, width = 6.69)
prp(tree, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1, cex = 0.6, 
    box.col = brewer.pal(9,"RdYlBu")[c(1,3,1,5,1,3,1,7,8)],
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Historical (1923) tree density \n(all trees > 15.2 cm)")
print(h, vp=viewport(.18, .25, .38, .45))
print(grid.text("a", x=unit(1, "npc"), y= unit (1, "npc"), 
               vp=viewport(.01, .9, .1, .1) ) )
dev.off()

##modern comparison
{
#Need to remove NA because there are three plots where the fuzzed FIA coordinates 
  #put the plot in the middle of the water, and BCM did not calculate climate vars for these plots.
tree_fia <- tree
tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "n" ] <-
  length(which(!is.na(d_fia[d_fia$ppt_mean8110<1299,"TPH_Tot"])))
tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "yval" ] <-
  mean(d_fia[d_fia$ppt_mean8110<1299,"TPH_Tot"], na.rm = T)
tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "n" ] <-
  length(which(!is.na(d_fia[d_fia$ppt_mean8110>=1299 & 
                 d_fia$spk_mean8110 >= 378,"TPH_Tot"])))
tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "yval" ] <-
  mean(d_fia[d_fia$ppt_mean8110>=1299 & 
               d_fia$spk_mean8110 >= 378,"TPH_Tot"], na.rm = T)
tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "n" ] <-
  length(which(!is.na(d_fia[d_fia$ppt_mean8110>=1299 & 
                 d_fia$spk_mean8110 < 378 & 
                 d_fia$cwd_mean8110 >= 587,"TPH_Tot"])))
tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "yval" ] <-
  mean(d_fia[d_fia$ppt_mean8110>=1299 & 
               d_fia$spk_mean8110 < 378 & 
               d_fia$cwd_mean8110 >= 587,"TPH_Tot"], na.rm = T)
tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "n" ] <-
  length(which(!is.na(d_fia[d_fia$ppt_mean8110>=1299 & 
                 d_fia$spk_mean8110 < 378 & 
                 d_fia$cwd_mean8110 < 587 & 
                 d_fia$spk_mean8110 >= 152,"TPH_Tot"])))
tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "yval" ] <-
  mean(d_fia[d_fia$ppt_mean8110>=1299 & 
               d_fia$spk_mean8110 < 378 & 
               d_fia$cwd_mean8110 < 587 & 
               d_fia$spk_mean8110 >= 152,"TPH_Tot"], na.rm = T)
tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "n" ] <-
  length(which(!is.na(d_fia[d_fia$ppt_mean8110>=1299 & 
                 d_fia$spk_mean8110 < 378 & 
                 d_fia$cwd_mean8110 < 587 & 
                 d_fia$spk_mean8110 < 152,"TPH_Tot"])))
tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "yval" ] <-
  mean(d_fia[d_fia$ppt_mean8110>=1299 & 
               d_fia$spk_mean8110 < 378 & 
               d_fia$cwd_mean8110 < 587 & 
               d_fia$spk_mean8110 < 152,"TPH_Tot"], na.rm = T)

}

h_fia <- #Histogram
  ggplot(d_fia)+
  geom_histogram(aes(TPH_Tot), binwidth = 30, center = 15,
                 fill=c(rep(brewer.pal(9,"RdYlBu")[1],2),
                        rep(brewer.pal(9,"RdYlBu")[2],1),
                        rep(brewer.pal(9,"RdYlBu")[3],1),
                        rep(brewer.pal(9,"RdYlBu")[5],1),
                        rep(brewer.pal(9,"RdYlBu")[7],1),
                        rep(brewer.pal(9,"RdYlBu")[8],4),
                        rep(brewer.pal(9,"RdYlBu")[9],16)
                 ),
                 col="black")+
  labs(x = "TPH", y = "Count")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

cairo_pdf("./Figures/El Dorado/Fig2b_v3.pdf", height = 3, width = 6.69)
prp(tree_fia, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1, cex = 0.6,
    box.col = brewer.pal(9,"RdYlBu")[c(1,8,1,8,1,9,1,9,9)],
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Modern (2001-2010) tree density \n(all trees > 15.2 cm)")
print(h_fia, vp=viewport(.18  , .25, .38, .45))
print(grid.text("b", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ) )
dev.off()
}

###3b. Small trees (15.2-30.5 cm)
{
##historical data
d$logTPH1 <- log10(d$TPH1+1)
m_tph1 <- glmulti(y="logTPH1", 
                 xr=potential_parms,
                 data=d,
                 level=1,method="h", plotty = FALSE) 
summary(m_tph1@objects[[1]])

tree <- #Best model according to glmulti. Using this one to create regression tree 
  rpart(m_tph1@objects[[1]]$formula, 
        method = "anova",
        data = d,
        control = rpart.control(cp = 0.020))
levels(tree$frame$var) <- c("<leaf>", "deficit", "precip","snowpack")
tree$frame$yval <- 10^(tree$frame$yval)-1 #reverse log-transform for regression tree

h <- #Histogram
  ggplot(d)+
  geom_histogram(aes(TPH1), binwidth = 30, center = 15,
                 fill=c(rep(brewer.pal(9,"RdYlBu")[1],1),
                        rep(brewer.pal(9,"RdYlBu")[2],1),
                        rep(brewer.pal(9,"RdYlBu")[3],1),
                        rep(brewer.pal(9,"RdYlBu")[5],1),
                        rep(brewer.pal(9,"RdYlBu")[7],1),
                        rep(brewer.pal(9,"RdYlBu")[8],1),
                        rep(brewer.pal(9,"RdYlBu")[9],6)
                 ),
                 col="black")+
  labs(x = "TPH", y = "Count")+
  coord_cartesian(xlim = c(0, 700)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

cairo_pdf("./Figures/El Dorado/FigA2a_v3.pdf", height = 4, width = 6)
prp(tree, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1,
    box.col = brewer.pal(9,"RdYlBu")[c(1,1,1,1,1,1,1,1,2)],
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Historical (1923) tree density \n(small trees 15.2-30.5 cm)")
print(h, vp=viewport(.25, .18, .48, .35))
print(grid.text("a", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ) )

dev.off()

##modern comparison
{
  d_fia$logTPH1 <- log10(d_fia$TPH1 + 1)
  #Need to remove NA because there are three plots where the fuzzed FIA coordinates 
  #put the plot in the middle of the water, and BCM did not calculate climate vars for these plots.
  tree_fia <- tree
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$ppt_mean8110<1325 &
                                d_fia$cwd_mean8110 >= 547, "logTPH1"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "yval" ] <- #Get modern mean
    10^mean(d_fia[d_fia$ppt_mean8110<1325 &
                 d_fia$cwd_mean8110 >= 547, "logTPH1"], na.rm = T) - 1
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$ppt_mean8110<1325 &
                                d_fia$cwd_mean8110 < 547, "logTPH1"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "yval" ] <- #Get modern mean
    10^mean(d_fia[d_fia$ppt_mean8110<1325 &
                    d_fia$cwd_mean8110 < 547, "logTPH1"], na.rm = T) - 1
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$ppt_mean8110>=1325 &
                                d_fia$spk_mean8110 >= 378, "logTPH1"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "yval" ] <- #Get modern mean
    10^mean(d_fia[d_fia$ppt_mean8110>=1325 &
                    d_fia$spk_mean8110 >= 378, "logTPH1"], na.rm = T) - 1
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$ppt_mean8110>=1325 &
                                d_fia$spk_mean8110 < 378 &
                                d_fia$cwd_mean8110>= 583, "logTPH1"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "yval" ] <- #Get modern mean
    10^mean(d_fia[d_fia$ppt_mean8110>=1325 &
                    d_fia$spk_mean8110 < 378 &
                    d_fia$cwd_mean8110>= 583, "logTPH1"], na.rm = T) - 1
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "n" ] <- #Get modern sample size
    length(which(!is.na(d_fia[d_fia$ppt_mean8110>=1325 &
                                d_fia$spk_mean8110 < 378 &
                                d_fia$cwd_mean8110 < 583, "logTPH1"])))
  tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "yval" ] <- #Get modern mean
    10^mean(d_fia[d_fia$ppt_mean8110>=1325 &
                    d_fia$spk_mean8110 < 378 &
                    d_fia$cwd_mean8110 < 583, "logTPH1"], na.rm = T) - 1
  

}

h_fia <- #Histogram
  ggplot(d_fia)+
  geom_histogram(aes(TPH1), binwidth = 30, center = 15,
                 fill=c(rep(brewer.pal(9,"RdYlBu")[1],1),
                        rep(brewer.pal(9,"RdYlBu")[2],1),
                        rep(brewer.pal(9,"RdYlBu")[3],1),
                        rep(brewer.pal(9,"RdYlBu")[5],1),
                        rep(brewer.pal(9,"RdYlBu")[7],1),
                        rep(brewer.pal(9,"RdYlBu")[8],1),
                        rep(brewer.pal(9,"RdYlBu")[9],17)
                 ),
                 col="black")+
  labs(x = "TPH", y = "Count")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

cairo_pdf("./Figures/El Dorado/FigA2b_v3.pdf", height = 4, width = 6)
prp(tree_fia, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1,
    box.col = brewer.pal(9,"RdYlBu")[c(1,1,3,5,1,2,1,6,6)],
    #values match tree$frame[,"yval"]
    clip.right.labs = FALSE, #Only applies if using type = 3
    mar = c(2,2,2,2), 
    main = "Modern (2001-2010) tree density \n(small trees 15.2-30.5 cm)")
print(h_fia, vp=viewport(.25, .18, .48, .35))
print(grid.text("b", x=unit(1, "npc"), y= unit (1, "npc"), 
                vp=viewport(.01, .9, .1, .1) ) )

dev.off()
}

###3b. Medium trees (30.5 - 61 cm)
{
  ##historical data
  #Not log-transforming
    m_tph2 <- glmulti(y="TPH2", 
                    xr=potential_parms,
                    data=d,
                    level=1,method="h", plotty = FALSE) 
  summary(m_tph2@objects[[1]])
  
  tree <- #Best model according to glmulti. Using this one to create regression tree 
    rpart(m_tph2@objects[[1]]$formula, 
          method = "anova",
          data = d,
          control = rpart.control(cp = 0.020))
  levels(tree$frame$var)[3] <- "precip"
  levels(tree$frame$var)[4] <- "snowpack"

  h <- #Histogram
    ggplot(d)+
    geom_histogram(aes(TPH2), binwidth = 15, center = 7.5,
                   fill=c(rep(brewer.pal(9,"RdYlBu")[1],1),
                          rep(brewer.pal(9,"RdYlBu")[2],1),
                          rep(brewer.pal(9,"RdYlBu")[3],1),
                          rep(brewer.pal(9,"RdYlBu")[5],1),
                          rep(brewer.pal(9,"RdYlBu")[7],1),
                          rep(brewer.pal(9,"RdYlBu")[8],1),
                          rep(brewer.pal(9,"RdYlBu")[9],6)
                   ),
                   col="black")+
    labs(x = "TPH", y = "Count")+
    coord_cartesian(xlim = c(0, 400)) +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, size = 9),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9))
  
  cairo_pdf("./Figures/El Dorado/FigA3a_v3.pdf", height = 4, width = 6)
  prp(tree, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1,
      box.col = brewer.pal(9,"RdYlBu")[c(1,3,1,1,3,4,4)],
      #values match tree$frame[,"yval"]
      clip.right.labs = FALSE, #Only applies if using type = 3
      mar = c(2,2,2,2), 
      main = "Historical (1923) tree density \n(medium trees 30.5-61 cm)")
  print(h, vp=viewport(.82, .18, .32, .35))
  print(grid.text("a", x=unit(1, "npc"), y= unit (1, "npc"), 
                  vp=viewport(.01, .9, .1, .1) ) )
  
  dev.off()
  
  ##modern comparison
{
    #Need to remove NA because there are three plots where the fuzzed FIA coordinates 
    #put the plot in the middle of the water, and BCM did not calculate climate vars for these plots.
    tree_fia <- tree
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$ppt_mean8110 < 1298, "TPH2"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$ppt_mean8110 < 1298, "TPH2"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$ppt_mean8110 >= 1298 &
                                  d_fia$aspect == "SW" &
                                  d_fia$spk_mean8110 >= 236, "TPH2"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$ppt_mean8110 >= 1298 &
                   d_fia$aspect == "SW" &
                   d_fia$spk_mean8110 >= 236, "TPH2"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$ppt_mean8110 >= 1298 &
                                  d_fia$aspect == "SW" &
                                  d_fia$spk_mean8110 < 236, "TPH2"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$ppt_mean8110 >= 1298 &
                   d_fia$aspect == "SW" &
                   d_fia$spk_mean8110 < 236, "TPH2"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$ppt_mean8110 >= 1298 &
                                  d_fia$aspect == "NE", "TPH2"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$ppt_mean8110 >= 1298 &
                   d_fia$aspect == "NE", "TPH2"], na.rm = T)
}
  
  h_fia <- #Histogram
    ggplot(d_fia)+
    geom_histogram(aes(TPH2), binwidth = 15, center = 7.5,
                   fill=c(rep(brewer.pal(9,"RdYlBu")[1],1),
                          rep(brewer.pal(9,"RdYlBu")[2],1),
                          rep(brewer.pal(9,"RdYlBu")[3],1),
                          rep(brewer.pal(9,"RdYlBu")[5],1),
                          rep(brewer.pal(9,"RdYlBu")[7],1),
                          rep(brewer.pal(9,"RdYlBu")[8],1),
                          rep(brewer.pal(9,"RdYlBu")[9],20)
                   ),
                   col="black")+
    labs(x = "TPH", y = "Count")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, size = 9),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9))
  
  cairo_pdf("./Figures/El Dorado/FigA3b_v3.pdf", height = 4, width = 6)
  prp(tree_fia, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1,
      box.col = brewer.pal(9,"RdYlBu")[c(1,9,1,1,9,9,9)],
      #values match tree$frame[,"yval"]
      clip.right.labs = FALSE, #Only applies if using type = 3
      mar = c(2,2,2,2), 
      main = "Modern (2001-2010) tree density \n(medium trees 30.5-61 cm)")
  print(h_fia, vp=viewport(.82, .18, .32, .35))
  print(grid.text("b", x=unit(1, "npc"), y= unit (1, "npc"), 
                  vp=viewport(.01, .9, .1, .1) ) )
  
  dev.off()
}

###3c. Large trees (>61 cm)
{
  ##historical data
  #Not log-transforming
  m_tph_large <- glmulti(y="TPH_large", 
                    xr=potential_parms,
                    data=d,
                    level=1,method="h", plotty = FALSE) 
  summary(m_tph_large@objects[[1]])
  
  tree <- #Best model according to glmulti. Using this one to create regression tree 
    rpart(m_tph_large@objects[[1]]$formula, 
          method = "anova",
          data = d,
          control = rpart.control(cp = 0.020))
  levels(tree$frame$var) <- c("<leaf>", "June deficit", "slope")
  
  h <- #Histogram
    ggplot(d)+
    geom_histogram(aes(TPH_large), binwidth = 15, center = 7.5,
                   fill=c(rep(brewer.pal(9,"RdYlBu")[1],1),
                          rep(brewer.pal(9,"RdYlBu")[2],1),
                          rep(brewer.pal(9,"RdYlBu")[3],1),
                          rep(brewer.pal(9,"RdYlBu")[5],1),
                          rep(brewer.pal(9,"RdYlBu")[7],1),
                          rep(brewer.pal(9,"RdYlBu")[8],1),
                          rep(brewer.pal(9,"RdYlBu")[9],7)
                   ),
                   col="black")+
    labs(x = "TPH", y = "Count")+
    coord_cartesian(xlim = c(0, 200)) +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, size = 9),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9))
  
  cairo_pdf("./Figures/El Dorado/Fig3a_v3.pdf", height = 4, width = 6)
  prp(tree, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1,
      box.col = brewer.pal(9,"RdYlBu")[c(1,3,1,3,4)],
      #values match tree$frame[,"yval"]
      clip.right.labs = FALSE, #Only applies if using type = 3
      mar = c(2,2,2,2), 
      main = "Historical (1923) tree density \n(large trees >61 cm)")
  print(h, vp=viewport(.18, .18, .32, .35))
  print(grid.text("a", x=unit(1, "npc"), y= unit (1, "npc"), 
                  vp=viewport(.01, .9, .1, .1) ) )
  
  dev.off()
  
  ##modern comparison
  {
    #Need to remove NA because there are three plots where the fuzzed FIA coordinates 
    #put the plot in the middle of the water, and BCM did not calculate climate vars for these plots.
    tree_fia <- tree
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$cwd_jun_mean8110 >= 84, "TPH_large"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$cwd_jun_mean8110 >= 84, "TPH_large"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$cwd_jun_mean8110 >= 84 &
                                  d_fia$slope >= 11, "TPH_large"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$cwd_jun_mean8110 >= 84 &
                   d_fia$slope >= 11, "TPH_large"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$cwd_jun_mean8110 >= 84 &
                                  d_fia$slope < 11, "TPH_large"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$cwd_jun_mean8110 >= 84 &
                   d_fia$slope >= 11, "TPH_large"], na.rm = T)
  }
  
  h_fia <- #Histogram
    ggplot(d_fia)+
    geom_histogram(aes(TPH_large), binwidth = 15, center = 7.5,
                   fill=c(rep(brewer.pal(9,"RdYlBu")[1],1),
                          rep(brewer.pal(9,"RdYlBu")[2],1),
                          rep(brewer.pal(9,"RdYlBu")[3],1),
                          rep(brewer.pal(9,"RdYlBu")[5],1),
                          rep(brewer.pal(9,"RdYlBu")[7],1),
                          rep(brewer.pal(9,"RdYlBu")[8],1),
                          rep(brewer.pal(9,"RdYlBu")[9],1)
                   ),
                   col="black")+
    labs(x = "TPH", y = "Count")+
    coord_cartesian(xlim = c(0, 200)) +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, size = 9),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9))
  
  cairo_pdf("./Figures/El Dorado/Fig3b_v3.pdf", height = 4, width = 6)
  prp(tree_fia, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1,
      box.col = brewer.pal(9,"RdYlBu")[c(1,3,1,3,3)],
      #values match tree$frame[,"yval"]
      clip.right.labs = FALSE, #Only applies if using type = 3
      mar = c(2,2,2,2), 
      main = "Modern (2001-2010) tree density \n(large trees >61 cm)")
  print(h_fia, vp=viewport(.18, .18, .32, .35))
  print(grid.text("b", x=unit(1, "npc"), y= unit (1, "npc"), 
                  vp=viewport(.01, .9, .1, .1) ) )
  
  dev.off()
}

##3d. Basal Area
{
  ##historical data
  #Not log-transforming
  m_ba <- glmulti(y="TotBA_met", 
                         xr=potential_parms,
                         data=d,
                         level=1,method="h", plotty = FALSE) 
  summary(m_ba@objects[[1]])
  
  tree <- #Best model according to glmulti. Using this one to create regression tree 
    rpart(m_ba@objects[[1]]$formula, 
          method = "anova",
          data = d,
          control = rpart.control(cp = 0.020))
  levels(tree$frame$var) <- c("<leaf>", "aspect", "deficit", "slope", "snowpack")
  
  h <- #Histogram
    ggplot(d)+
    geom_histogram(aes(TotBA_met), binwidth = 15, center = 7.5,
                   fill=c(rep(brewer.pal(9,"RdYlBu")[1],1),
                          rep(brewer.pal(9,"RdYlBu")[2],1),
                          rep(brewer.pal(9,"RdYlBu")[3],1),
                          rep(brewer.pal(9,"RdYlBu")[5],1),
                          rep(brewer.pal(9,"RdYlBu")[7],1),
                          rep(brewer.pal(9,"RdYlBu")[8],1),
                          rep(brewer.pal(9,"RdYlBu")[9],5)
                   ),
                   col="black")+
    labs(x = expression(paste("Basal area (", m^2, ha^-1,")")),
         y = "Count")+
    coord_cartesian(xlim = c(0, 175)) +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, size = 9),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9))
  
  cairo_pdf("./Figures/El Dorado/Fig4a_v3.pdf", height = 3, width = 6.69)
  prp(tree, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1, cex = 0.6,
      box.col = brewer.pal(9,"RdYlBu")[c(1,3,1,3,1,4,1,4,6)],
      #values match tree$frame[,"yval"]
      clip.right.labs = FALSE, #Only applies if using type = 3
      mar = c(2,2,2,2), 
      main = "Historical (1923) tree basal area \n(all trees >15.2 cm)")
  print(h, vp=viewport(.18, .18, .32, .35))
  print(grid.text("a", x=unit(1, "npc"), y= unit (1, "npc"), 
                  vp=viewport(.01, .9, .1, .1) ) )
  dev.off()
  
  ##modern comparison
  {
    #Need to remove NA because there are three plots where the fuzzed FIA coordinates 
    #put the plot in the middle of the water, and BCM did not calculate climate vars for these plots.
    tree_fia <- tree
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$aspect == "SW", "TotBA_met"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$aspect == "SW", "TotBA_met"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$aspect == "NE" &
                                  d_fia$slope >= 12, "TotBA_met"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$aspect == "NE" &
                   d_fia$slope >= 12, "TotBA_met"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$aspect == "NE" &
                                  d_fia$slope < 12  &
                                  d_fia$cwd_mean8110 >= 441, "TotBA_met"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$aspect == "NE"&
                   d_fia$slope < 12  &
                   d_fia$cwd_mean8110 >= 441, "TotBA_met"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$aspect == "NE" &
                                  d_fia$slope < 12  &
                                  d_fia$cwd_mean8110 < 441 &
                                  d_fia$spk_mean8110 >= 438, "TotBA_met"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$aspect == "NE"&
                   d_fia$slope < 12  &
                   d_fia$cwd_mean8110 < 441  &
                   d_fia$spk_mean8110 >= 438, "TotBA_met"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$aspect == "NE" &
                                  d_fia$slope < 12  &
                                  d_fia$cwd_mean8110 < 441 &
                                  d_fia$spk_mean8110 < 438, "TotBA_met"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$aspect == "NE"&
                   d_fia$slope < 12  &
                   d_fia$cwd_mean8110 < 441  &
                   d_fia$spk_mean8110 < 438, "TotBA_met"], na.rm = T)
  }
  
  h_fia <- #Histogram
    ggplot(d_fia)+
    geom_histogram(aes(TotBA_met), binwidth = 15, center = 7.5,
                   fill=c(rep(brewer.pal(9,"RdYlBu")[1],1),
                          rep(brewer.pal(9,"RdYlBu")[2],1),
                          rep(brewer.pal(9,"RdYlBu")[3],1),
                          rep(brewer.pal(9,"RdYlBu")[5],1),
                          rep(brewer.pal(9,"RdYlBu")[7],1),
                          rep(brewer.pal(9,"RdYlBu")[8],1),
                          rep(brewer.pal(9,"RdYlBu")[9],1)
                   ),
                   col="black")+
    labs(x = expression(paste("Basal area (", m^2, ha^-1,")")),
         y = "Count")+
    coord_cartesian(xlim = c(0, 175)) +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, size = 9),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9))
  
  cairo_pdf("./Figures/El Dorado/Fig4b_v3.pdf", height = 3, width = 6.69)
  prp(tree_fia, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1, cex = 0.6,
      box.col = brewer.pal(9,"RdYlBu")[c(1,3,1,3,1,3,1,3,3)],
      #values match tree$frame[,"yval"]
      clip.right.labs = FALSE, #Only applies if using type = 3
      mar = c(2,2,2,2), 
      main = "Modern (2001-2010) basal area \n(all trees >15.2 cm)")
  print(h_fia, vp=viewport(.18, .18, .32, .35))
  print(grid.text("b", x=unit(1, "npc"), y= unit (1, "npc"), 
                  vp=viewport(.01, .9, .1, .1) ) )
  dev.off()
}

##3e. Fir:pine
{
  ##historical data
  #Not log-transforming
  m_ab_pi <- glmulti(y="AB_PI", 
                  xr=potential_parms,
                  data=d,
                  level=1,method="h", plotty = FALSE) 
  summary(m_ab_pi@objects[[1]])
  
  tree <- #Best model according to glmulti. Using this one to create regression tree 
    rpart(m_ab_pi@objects[[1]]$formula, 
          method = "anova",
          data = d,
          control = rpart.control(cp = 0.025))
  #NOTE cp = 0.025 to simplify this tree; different from other models.
  levels(tree$frame$var) <- c("<leaf>", "aspect", "deficit", "elevation", "precip")
  
  h <- #Histogram
    ggplot(d)+
    geom_histogram(aes(AB_PI), binwidth = 0.1, center = 0.05,
                   fill=c(rep(brewer.pal(9,"RdYlBu")[1],2),
                          rep(brewer.pal(9,"RdYlBu")[2],1),
                          rep(brewer.pal(9,"RdYlBu")[3],1),
                          rep(brewer.pal(9,"RdYlBu")[4],1),
                          rep(brewer.pal(9,"RdYlBu")[6],1),
                          rep(brewer.pal(9,"RdYlBu")[7],1),
                          rep(brewer.pal(9,"RdYlBu")[8],1),
                          rep(brewer.pal(9,"RdYlBu")[9],2)
                   ),
                   col="black")+
    labs(x = "Fir:pine ratio", y = "Count")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, size = 9),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9))
  
  cairo_pdf("./Figures/El Dorado/Fig5a_v3.pdf", height = 3, width = 6.69)
  prp(tree, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1, cex = 0.6,
      box.col = brewer.pal(9,"RdYlBu")[c(1,1,1,3,4,1,2,1,4,7,8)],
      #values match tree$frame[,"yval"]
      clip.right.labs = FALSE, #Only applies if using type = 3
      mar = c(2,2,2,2), 
      main = "Historical (1923) tree fir-pine ratio \n(all trees >15.2 cm)")
  print(h, vp=viewport(.21, .15, .42, .30))
  print(grid.text("a", x=unit(1, "npc"), y= unit (1, "npc"), 
                  vp=viewport(.01, .9, .1, .1) ) )
  dev.off()
  
  ##modern comparison
  {
    #Need to remove NA because there are three plots where the fuzzed FIA coordinates 
    #put the plot in the middle of the water, and BCM did not calculate climate vars for these plots.
    tree_fia <- tree
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$elev_mean < 1996 & 
                                  d_fia$aspect == "SW" & 
                                  d_fia$ppt_mean8110 < 1298, "AB_PI"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[1], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$elev_mean < 1996 & 
                   d_fia$aspect == "SW" & 
                   d_fia$ppt_mean8110 < 1298, "AB_PI"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$elev_mean < 1996 & 
                                  d_fia$aspect == "SW" & 
                                  d_fia$ppt_mean8110 >= 1298, "AB_PI"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[2], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$elev_mean < 1996 & 
                   d_fia$aspect == "SW" & 
                   d_fia$ppt_mean8110 >= 1298, "AB_PI"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$elev_mean < 1996 & 
                                  d_fia$aspect == "NE" & 
                                  d_fia$cwd_mean8110 >= 587, "AB_PI"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[3], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$elev_mean < 1996 & 
                   d_fia$aspect == "NE" & 
                   d_fia$cwd_mean8110 >= 587, "AB_PI"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$elev_mean < 1996 & 
                                  d_fia$aspect == "NE" & 
                                  d_fia$cwd_mean8110 < 587 & 
                                  d_fia$ppt_mean8110 >= 1642, "AB_PI"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[4], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$elev_mean < 1996 & 
                   d_fia$aspect == "NE" & 
                   d_fia$cwd_mean8110 < 587 & 
                   d_fia$ppt_mean8110 >= 1642, "AB_PI"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$elev_mean < 1996 & 
                                  d_fia$aspect == "NE" & 
                                  d_fia$cwd_mean8110 < 587 & 
                                  d_fia$ppt_mean8110 < 1642, "AB_PI"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[5], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$elev_mean < 1996 & 
                   d_fia$aspect == "NE" & 
                   d_fia$cwd_mean8110 < 587 & 
                   d_fia$ppt_mean8110 < 1642, "AB_PI"], na.rm = T)
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[6], "n" ] <- #Get modern sample size
      length(which(!is.na(d_fia[d_fia$elev_mean > 1996, "AB_PI"])))
    tree_fia$frame[which(tree_fia$frame$var == "<leaf>")[6], "yval" ] <- #Get modern mean
      mean(d_fia[d_fia$elev_mean > 1996, "AB_PI"], na.rm = T)
  }
  
  ##What's up with the wierd NE, low-deficit, high-precip pine-dominated plots?
  View(d[d$elev_mean < 1996 & 
               d$aspect == "NE" & 
               d$cwd_mean8110 < 587 & 
               d$ppt_mean8110 >= 1642, ])
  View(d_fia[d_fia$elev_mean < 1996 & 
               d_fia$aspect == "NE" & 
               d_fia$cwd_mean8110 < 587 & 
               d_fia$ppt_mean8110 >= 1642, ])
  ##Nothing obvious jumps out; some PICO plots, some PILA, some YP
  
  h_fia <- #Histogram
    ggplot(d_fia)+
    geom_histogram(aes(AB_PI), binwidth = 0.1, center = 0.05,
                   fill=c(rep(brewer.pal(9,"RdYlBu")[1],2),
                          rep(brewer.pal(9,"RdYlBu")[2],1),
                          rep(brewer.pal(9,"RdYlBu")[3],1),
                          rep(brewer.pal(9,"RdYlBu")[4],1),
                          rep(brewer.pal(9,"RdYlBu")[6],1),
                          rep(brewer.pal(9,"RdYlBu")[7],1),
                          rep(brewer.pal(9,"RdYlBu")[8],1),
                          rep(brewer.pal(9,"RdYlBu")[9],2)
                   ),
                   col="black")+
    labs(x = "Fir:pine ratio", y = "Count")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, size = 9),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9))

  cairo_pdf("./Figures/El Dorado/Fig5b_v3.pdf", height = 3, width = 6.69)
  prp(tree_fia, varlen = 0, faclen = 0, type = 3, ge = " ≥ ", extra = 1, cex = 0.6,
      box.col = brewer.pal(9,"RdYlBu")[c(1,1,1,6,6,1,3,1,9,6,7)],
      #values match tree$frame[,"yval"]
      clip.right.labs = FALSE, #Only applies if using type = 3
      mar = c(2,2,2,2), 
      main = "Modern (2001-2010) fir-pine ratio \n(large trees >61 cm)")
  print(grid.text("b", x=unit(1, "npc"), y= unit (1, "npc"), 
                  vp=viewport(.01, .9, .1, .1) ) )
  print(h_fia, vp=viewport(.21, .15, .42, .30))
  dev.off()
}



####4. Maps####
summary(m_tph@objects[[1]])
#Used "Align Rasters" tool in QGIS to create the below layers

cwd <- raster("../Large Files/GIS/El Dorado/AlignedLayers/cwd_Align.tif") #From http://climate.calcommons.org/node/1129 CA Climate Commons, BCM. 30 year water-year mean annual climatic water deficit values 1981-2010. EPSG 3310 NAD83 CA Albers
aet <- raster("../Large Files/GIS/El Dorado/AlignedLayers/aet_Align.tif") #mean actual evapotranspiration values 
spk <- raster("../Large Files/GIS/El Dorado/AlignedLayers/spk_Align.tif") #mean April 1 snowpack values
ppt <- raster("../Large Files/GIS/El Dorado/AlignedLayers/ppt_Align.tif") #mean annual precipitation values
tmx <- raster("../Large Files/GIS/El Dorado/AlignedLayers/tmx_Align.tif") #Mean annual max temperatures 1981-2010.
slp <- raster("../Large Files/GIS/El Dorado/AlignedLayers/slp_Align.tif") #Slope in degrees; resampled to 270m res.
asp <- raster("../Large Files/GIS/El Dorado/AlignedLayers/asp_Align.tif") #Aspect in degrees; resampled to 270m res.

m_tph@objects[[1]]$formula
s <- stack(cwd, aet, spk, ppt, tmx, slp, asp)
df <- as.data.frame(getValues(s))
df[df<0] <- NA
r <- 1
for(r in 1:nrow(df)){
  if(any(is.na(df[r,]))){
    df[r,] <- NA
  }
}
names_tmp <- names(df)
names(df) <- c("cwd_mean8110", "aet_mean8110", "spk_mean8110", "ppt_mean8110", "tmx_mean8110", "slope", "aspect")
df$aspect_cat <- 
  ifelse(between(df$aspect, 135,315),"SW","NE")
df$aspect <- factor(df$aspect_cat)

df$pred_tph <- predict.lm(m_tph@objects[[1]], newdata = df)

df[which(!between(df$cwd_mean8110,range(d$cwd_mean8110)[1],range(d$cwd_mean8110)[2])), ] <-
  NA
df[which(!between(df$aet_mean8110,range(d$aet_mean8110)[1],range(d$aet_mean8110)[2])), ] <-
  NA
df[which(!between(df$spk_mean8110,range(d$spk_mean8110)[1],range(d$spk_mean8110)[2])), ] <-
  NA
df[which(!between(df$ppt_mean8110,range(d$ppt_mean8110)[1],range(d$ppt_mean8110)[2])), ] <-
  NA
df[which(!between(df$tmx_mean8110,range(d$tmx_mean8110)[1],range(d$tmx_mean8110)[2])), ] <-
  NA
#df[which(!between(df$slope_mean8110,range(d$slope_mean8110)[1],range(d$slope_mean8110)[2])), ] <-NA
#df[which(!between(df$aspect_mean8110,range(d$aspect_mean8110)[1],range(d$aspect_mean8110)[2])), ] <-NA
df[which(df$pred_tph<1), ] <-NA #Model fit gives some negative values at margins still. 


pred_Align <- cwd
pred_Align <- setValues(x = pred_Align, values = df$pred_tph)
range(getValues(pred_Align), na.rm = T); range(df$pred_tph, na.rm = T)
plot(pred_Align)

writeRaster(pred_Align, "../Large Files/GIS/El Dorado/TPH_Predicted.tif", overwrite = TRUE)


####5. Appendix Table####
pred_names <- c("cwd_mean8110", "cwd_jun_mean8110", "aet_mean8110", "spk_mean8110", 
                "ppt_mean8110", "tmx_mean8110", "elev_mean","slope", "aspectSW")
response_vars <- c("TPH all", "TPH small", "TPH med", "TPH large", "BA", "FPR")
t_a1 <- data.frame(Response = rep(NA, 1), "R_Squared" = rep(NA, 1) )
t_a1[,pred_names] <- NA  

m_list <- list(m_tph,m_tph1,m_tph2,m_tph_large,m_ba,m_ab_pi)
for(m in c(1:length(m_list))){
  tmp <- as.data.frame(t(summary(m_list[[m]]@objects[[1]])$coefficients))
  tmp[5,] <- paste0(
    round(tmp[1,],3)," (",
    round(tmp[2,],3), ")"
  )
  t_a1 <- bind_rows(t_a1,tmp[5,])
  t_a1[m+1,"R_Squared"] <- round(r.squaredGLMM(m_list[[m]]@objects[[1]]) [1],2)
  t_a1[m+1,"Response"] <- response_vars[m]
}
t_a1 <- t_a1[-1,-12]
rownames(t_a1) <- paste("model", seq(1:6))
t_a1 <- as.data.frame(t(t_a1),stringsAsFactors = FALSE)

t_a1[1,] <- c("Total TPH \n>15 cm", "TPH \n15-30 cm", "TPH \n30-60 cm", "TPH \n >60 cm", 
              "Total BA", "Mean Abies:Pinus \nratio")
rownames(t_a1)[3:11] <- c( "Mean annual\nCWD (mm)", "Mean June\nCWD (mm)", "Mean annual\nAET (mm)", 
                           "Mean April 1\nsnowpack (mm)", "Mean annual\nprecip (mm)", "Mean annual\nhigh temp (C)",
                           "Mean \nelevation (m)", "Mean \nslope (deg)", "SW aspect \n (compared to NE)")
#write_tableHTML(tableHTML(t_a1), file = './Tables/Table A1.html')