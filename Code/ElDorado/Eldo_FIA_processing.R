library(rgdal) #for readOGR
library(tidyverse) #for dplyer::between

####1. Read in data####
#Read plots from west-side El Dorado NF (layer created manually in QGIS from CA_PLOTSNAP.csv)
FIA <- readOGR("../Large Files/GIS/El Dorado/Modern/ENF FIA plots.shp")
#plot(FIA)

#Read in FIA condition and tree list data
FIA_TREE <- read.csv("/Users/Jens/Documents/Davis/Post-Doc/Odion Response/FIA Analysis/Raw/CA_TREE.CSV")
FIA_COND <- read.csv("/Users/Jens/Documents/Davis/Post-Doc/Odion Response/FIA Analysis/Raw/CA_COND.CSV")
FIA_COND <- FIA_COND[,-which(sapply(FIA_COND, function(x)all(is.na(x))))] #Delete empty columns
FIA_TREE <- FIA_TREE[,-which(sapply(FIA_TREE, function(x)all(is.na(x))))] #Delete empty columns
fia_forest_types <- read.csv("./Data/Raw/fia_forest_types.csv")
fia_species_list <- read.csv("./Data/Raw/fia_species_list.csv")

#Process data from CA_PLOTSNAP; pull out relevant columns
d_FIA <- FIA@data[,c("CN","PREV_PLT_C","INVYR","PLOT","LAT","LON","ELEV")]
d_FIA <- d_FIA[-which(duplicated(d_FIA$CN)),] #CA_PLOTSNAP (ENF FIA plots.shp) has duplicates; not sure why.
d_FIA$CN <- #Complicated regex to convert factor CN to string CN
  as.character(unlist(regmatches(d_FIA$CN, gregexpr("[[:digit:]]+", d_FIA$CN))))
d_FIA$ELEV_met <- as.double(as.character(d_FIA$ELEV)) * 0.3048
d_FIA$INVYR <- as.numeric(as.character(d_FIA$INVYR))

treematch <- condmatch <- list()

####2. Aggregate FIA data to the plot-level####
for(i in 1:nrow(d_FIA)){
  ##Aggregate condition-level data to plot-level
  condmatch[[i]] <- #Identify the conditions matching the Plot CN in question 
    #Could be multiple rows for multiple conditions
    grep(paste0("^",d_FIA$CN[i],"$"),as.character(FIA_COND$PLT_CN))
  d_cond_tmp <- #Make temporary dataset of just those rows (a shortcut for filling out attributes below)
    FIA_COND[condmatch[[i]],]
  #View(d_cond_tmp[,-which(sapply(d_cond_tmp, function(x)all(is.na(x)))),] ) #Inspect the conditions in your plot
  d_FIA[i,"PLOT_cond"] <- #Get the Plot ID; should just be a single number (this is just a check)
    mean(d_cond_tmp$PLOT)
  d_FIA[i,"STDAGE_cond"] <- #Get the stand age; multiple numbers for multiple conditions
    paste(d_cond_tmp$STDAGE,collapse = "-")
  d_FIA[i,"FORTYPCD_cond"] <- #Get the forest type for each condition
    paste(fia_forest_types[pmatch(d_cond_tmp$FORTYPCD,fia_forest_types$FORTYPCD),"MEANING"],collapse = "-")
  d_FIA[i,"OWNGRPCD_cond"] <- #Get the forest type for each condition
    paste(d_cond_tmp$OWNGRPCD,collapse = "-")
  d_FIA[i,"DSTRBCD_cond"] <- #Get disturbance codes (two most prominent disturbances)
    #Codes 10-12 = insects, 20-22 = disease; 30-32 = fire (32 = crown fire)
    paste(c(d_cond_tmp$DSTRBCD1),collapse = "-") #Inspected DSTRBCD2, but didn't add anything.
  d_FIA[i,"CONDPROP_cond"] <- #Get the proportion of the plot in each condition
    paste(c(d_cond_tmp$CONDPROP_UNADJ),collapse = "-")
  #d_FIA[i,"BALIVE_wt_cond_sqftac"] <- #Weight the live basal area of each condition, 
    #by the proportion of the plot in each condition
    #This includes trees <6 inches, so we won't analyze it.
  #  weighted.mean(d_cond_tmp$BALIVE,d_cond_tmp$CONDPROP_UNADJ)
  
  ##Aggregate tree-level data to plot level
  treematch[[i]] <- #Identify rows for a given plot; each row = a tree
    grep(paste0("^",d_FIA$CN[i],"$"),as.character(FIA_TREE$PLT_CN))
  d_tree_tmp <- FIA_TREE[treematch[[i]],] #Object for the tree data. 
  d_tree_tmp <- #Subset temporary dataset to only live trees >= 6 inches
    d_tree_tmp[d_tree_tmp$STATUSCD==1 & d_tree_tmp$DIA>=6,] 
  #View(d_tree_tmp[,-which(sapply(d_tree_tmp, function(x)all(is.na(x)))),] ) #Inspect the trees in your plot
  
  d_FIA[i,"PLOT_tree"] <- mean(d_tree_tmp$PLOT)
  d_FIA[i,"DENS_6plus"] <- sum(d_tree_tmp$TPA_UNADJ) / 0.4047 #Convert to ha
  d_FIA[i,"DENS_6_12"] <- sum(d_tree_tmp[which(between(d_tree_tmp$DIA,5.99,11.99)),"TPA_UNADJ"]) / 0.4047
  d_FIA[i,"DENS_12_24"] <- sum(d_tree_tmp[which(between(d_tree_tmp$DIA,11.99,23.99)),"TPA_UNADJ"]) / 0.4047
  d_FIA[i,"DENS_24_36"] <- sum(d_tree_tmp[which(between(d_tree_tmp$DIA,23.99,35.99)),"TPA_UNADJ"]) / 0.4047
  d_FIA[i,"DENS_36plus"] <- sum(d_tree_tmp[which(d_tree_tmp$DIA>35.99),"TPA_UNADJ"]) / 0.4047
  d_FIA[i,"BALIVE_sqftac"] <- sum(((pi*(d_tree_tmp$DIA/2)^2)*0.00694444)*d_tree_tmp$TPA_UNADJ) 
  d_FIA[i,"BALIVE_sqmha"] <- d_FIA[i,"BALIVE_sqftac"] * 0.229568 #Convert sq ft/ac to sq m/ha
  d_FIA[i,"BALIVE_sqmha_check"] <- NA
  #Above: area in square inches, converted to square feet, times TPA estimate for that tree = ft2/ac; 
  #Then covert to m2/ha
  #Then set up a dummy column that will be filled below.
  
  ##Calculate basal area by species
  d_FIA[i,"ABCO_met"] <- sum(((pi*(d_tree_tmp[d_tree_tmp$SPCD==15,"DIA"]/2)^2)*0.00694444)
                             *d_tree_tmp[d_tree_tmp$SPCD==15,"TPA_UNADJ"])  * 0.229568 #Convert sq ft/ac to sq m/ha
  d_FIA[i,"ABMA_met"] <- sum(((pi*(d_tree_tmp[d_tree_tmp$SPCD==20,"DIA"]/2)^2)*0.00694444)
                             *d_tree_tmp[d_tree_tmp$SPCD==20,"TPA_UNADJ"])  * 0.229568 
  d_FIA[i,"CADE_met"] <- sum(((pi*(d_tree_tmp[d_tree_tmp$SPCD==81,"DIA"]/2)^2)*0.00694444)
                             *d_tree_tmp[d_tree_tmp$SPCD==81,"TPA_UNADJ"])  * 0.229568 
  d_FIA[i,"JUNIPER_met"] <- 
    sum(
      ((pi*(
        d_tree_tmp[which(d_tree_tmp$SPCD%in%c(64,66,57,68,61,59,63,60,69,65,58)),"DIA"]
        /2)^2)*0.00694444) * 
        d_tree_tmp[which(d_tree_tmp$SPCD%in%c(64,66,57,68,61,59,63,60,69,65,58)),"TPA_UNADJ"]
    ) * 0.229568 
  d_FIA[i,"PICO_met"] <- sum(((pi*(d_tree_tmp[d_tree_tmp$SPCD==108,"DIA"]/2)^2)*0.00694444)
                             *d_tree_tmp[d_tree_tmp$SPCD==108,"TPA_UNADJ"])  * 0.229568 
  d_FIA[i,"PIJE_met"] <- sum(((pi*(d_tree_tmp[d_tree_tmp$SPCD==116,"DIA"]/2)^2)*0.00694444)
                             *d_tree_tmp[d_tree_tmp$SPCD==116,"TPA_UNADJ"])  * 0.229568 
  d_FIA[i,"PILA_met"] <- sum(((pi*(d_tree_tmp[d_tree_tmp$SPCD==117,"DIA"]/2)^2)*0.00694444)
                             *d_tree_tmp[d_tree_tmp$SPCD==117,"TPA_UNADJ"])  * 0.229568 
  d_FIA[i,"PIMO_met"] <- sum(((pi*(d_tree_tmp[d_tree_tmp$SPCD==119,"DIA"]/2)^2)*0.00694444)
                             *d_tree_tmp[d_tree_tmp$SPCD==119,"TPA_UNADJ"])  * 0.229568 
  d_FIA[i,"PIPO_met"] <- sum(((pi*(d_tree_tmp[d_tree_tmp$SPCD==122,"DIA"]/2)^2)*0.00694444)
                             *d_tree_tmp[d_tree_tmp$SPCD==122,"TPA_UNADJ"])  * 0.229568 
  d_FIA[i,"PSME_met"] <- sum(((pi*(d_tree_tmp[d_tree_tmp$SPCD==202,"DIA"]/2)^2)*0.00694444)
                             *d_tree_tmp[d_tree_tmp$SPCD==202,"TPA_UNADJ"])  * 0.229568 
  d_FIA[i,"QUCH_met"] <- sum(((pi*(d_tree_tmp[d_tree_tmp$SPCD==805,"DIA"]/2)^2)*0.00694444)
                             *d_tree_tmp[d_tree_tmp$SPCD==805,"TPA_UNADJ"])  * 0.229568 
  d_FIA[i,"QUKE_met"] <- sum(((pi*(d_tree_tmp[d_tree_tmp$SPCD==818,"DIA"]/2)^2)*0.00694444)
                             *d_tree_tmp[d_tree_tmp$SPCD==818,"TPA_UNADJ"])  * 0.229568 
  d_FIA[i,"BALIVE_sqmha_check"] <- 
    sum(d_FIA[i,pmatch(c("ABCO","ABMA","CADE","JUNIPER","PICO","PIJE","PILA","PIMO","PIPO","PSME","QUCH","QUKE"),
                       names(d_FIA))])
}

####3. Filter and refine the FIA data to the appropriate plots
d_FIA_tmp <-
  d_FIA %>% #start with 258 plots
  filter(between(INVYR,2001,2010))  #Plots between 2001 and 2010, inclusive (1990's plots unreliable, remaining N 176)
#Removing plots from 1994 because:
#1) were all fuzzed 1 mile (FIA user guide) vs 0.5 miles for more recent plots,
#2) were all on private land (so there may be correlated bias between ownership and sampling year)
#3) Contain a majority of the plots with >150 m elevation difference between FIA-reported and extracted elev. estimates
#Removing plots from 2011 and beyond because:
#1) These were all resamples of plots from 2001-2010.

d_FIA_tmp <-
  d_FIA_tmp %>% #start with 176 plots
  filter(between(ELEV_met,1229.523,2438.279))  #Plots within the elevation range of timber inventory data (Remaining N=117)

d_FIA_tmp <-
  d_FIA_tmp %>% #Start with 117 plots
  filter(BALIVE_sqmha > 9) # 9 m2/ha is a natural breakpoint in the BA data, which removes the following 19 plots:
#Nonstocked plots (N=8),
#Young stands with stand age <25; likely plantations with no disturbance indicated (N = 9; 1 of which had "grazing" indicated),
#Plots with insect damage (N = 2),
#And one plots with no identifiable reason for the low basal area (N = 1)
#Remaining N = 98

d_FIA_tmp <- 
  #Remove plots with fire damage in disturbance field (N = 3)
  d_FIA_tmp[-c(grep(c(31),d_FIA_tmp$DSTRBCD_cond),grep(c(32),d_FIA_tmp$DSTRBCD_cond)),]
d_FIA_tmp <- 
  #Remove plots in Mountain Hemlock forest type (N = 1)
  d_FIA_tmp[-which(d_FIA_tmp$FORTYPCD_cond=="Mountain hemlock"),]

##Final N = 94

####3. Write data####
write_csv(d_FIA_tmp,"./Data/Derived/eldo_FIA_processed.csv")

FIA <- FIA[-which(duplicated(FIA$CN)),] #Remove duplicates from shapefile
FIA$CN <- as.character(unlist(regmatches(FIA$CN, gregexpr("[[:digit:]]+", FIA$CN)))) #Replicate complex regex from first section.

FIA <- FIA[which(FIA$CN%in%d_FIA_tmp$CN),] #Make new shapefile with only the plots that were filtered out in section 2. 
writeOGR(FIA,"../Large Files/GIS/El Dorado/Modern","ENF_FIA_subset",driver = "ESRI Shapefile") #EPSG 4269