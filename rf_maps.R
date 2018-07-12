############################################
## Maya Fraser
## 4/7/16
## create maps of china province risk factors for all provinces
############################################


##################
## Set up R
##################

rm(list=ls())
library(foreign); library(plyr); library(data.table); library(lattice); library(latticeExtra); library(RColorBrewer);library(ggplot2)
library(maptools) ; library(RMySQL)
windows <- Sys.info()[1]=="Windows"
root <- ifelse(Sys.info()[1]=="Windows","J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

#source in functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_outputs.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))

# read in shape files 
provinces <- readShapeSpatial(paste0(root, "/DATA/SHAPE_FILES/GBD_geographies/selected_subnationals/CHN/ADM1/GIS/CHN_adm1.shp"))
provinces <- fortify(provinces, region="location_i")
provinces$id <- as.integer(provinces$id)

# Get location metadata to merge onto data
locations <- get_location_metadata(location_set_id = 22)
locations <- locations[,c("ihme_loc_id", "location_id", "parent_id")]

#Generate list of relevant provinces
prov.list <- setdiff(c(locations[parent_id == 6, location_id], locations[parent_id == 44533, location_id]), 44533)
remove <- c(361, 354) # HK and Macao
prov.list <- setdiff(prov.list, remove)

#Pull burden data
risk_ids <- c(86, 87, 83, 84, 240, 241, 94, 136, 137, 238, 334, 335, 100)
risk_ids <- c(240, 334, 335) #Top 3 in terms of death-Wasting, LBW, short gest
#risk_ids <- c(86,87,100) # Ranked 4-6 in terms of 2016 deaths
#risk_ids <- c(136,94,241)
    # 334 (short gest), 335 (lbw), 341(imp kidney)
years <- c(1990, 2016)
all.mr <- get_outputs(topic = "rei", location_id = prov.list, rei_id = risk_ids, year_id = years, measure_id = 1, metric_id = 3, 
    age_group_id = 1, sex_id = c(3), version = "latest", gbd_round_id = 4)
all.mr[, rate_per100000 := val * 100000]
all.mr <- all.mr[, .(year_id, location_name, rei_name, rate_per100000)]
#all.mr <- dcast(all.mr, location_id + year_id + location_name~rei_name, value.var ="rate")
setnames(all.mr, "location_id", "id")
#Merge onto shapefile
data <- merge(provinces, all.mr, by=c("id"), all.x=T)

################################################
### print the maps
################################################
# set the colors
rbPal <- colorRampPalette(c("dark green", "yellow","red"))
colors <- rbPal(5)

# actually plot
pdf(paste0(root, "temp/", user, "/chn/China_maps_rate_wrap_hi_sex2.pdf",sep=""),width=12,height=8)

### loop through the years
#for(year in c(1990, 2005, 2016)) {
    #for(sex in 1:2){
        #data_temp <- data[data$year_id == year,]
        data_temp <- data[complete.cases(data$rei_name),]
        data_temp <- data_temp[complete.cases(data_temp$sex_id),] ##CHECK on this
       #sex.name <- ifelse(sex == 1, "Males", "Females")
        ylim <- range(data_temp$rate, na.rm=TRUE)
        ylim <- c(floor(ylim[1]), ceiling(ylim[2]))
        # order the variables to make sure the graphing doesn't get messed up
        data_temp <- data_temp[order(data_temp$id, data_temp$group, data_temp$piece, data_temp$order),]    
        title = paste("Attributable Mortality for risk factors in Children Under 5 by sex", sep=" ")
        
        print(ggplot(data_temp) +
                geom_polygon(aes(x=long, y=lat, group=group, fill=rate)) +
                scale_fill_gradientn(colours=colors, limits=ylim)  + 
                geom_path(data=provinces, aes(x=long, y=lat, group=group)) + 
                scale_x_continuous("", breaks=NULL) + 
                scale_y_continuous("", breaks=NULL) + 
                coord_fixed(ratio=1) + 
                facet_wrap(~sex_name + rei_name) +
                guides(fill=guide_colourbar(title="Deaths/100,000", barheight=10)) + 
                theme_bw(base_size=10) +  
                labs(title=paste(title, sep="")))   
  #}    
  #print(paste0("printed ", year))
#}
dev.off()


#Reshape and gen percent change 1990-2016 map
df <- dcast(all.mr, location_id + location_name ~ year_id, 
              value.var = "rate")
setnames(df, c("1990", "2016"), c("val_1990", "val_2016"))
df <- df[,change := ((val_2016-val_1990)/val_1990)*100]
setnames(df, "location_id", "id")

#Merge onto shapefile
data_temp <- merge(provinces, df, by=c("id"), all.x=T)

# set the colors
rbPal <- colorRampPalette(c("dark green", "yellow","red"))
colors <- rbPal(5)

# actually plot
pdf(paste0(root, "temp/", user, "/chn/China_maps_rate_change.pdf",sep=""),width=12,height=8)
ylim <- range(data_temp$change, na.rm=TRUE)
ylim <- c(floor(ylim[1]), ceiling(ylim[2]))
# order the variables to make sure the graphing doesn't get messed up
data_temp <- data_temp[order(data_temp$id, data_temp$group, data_temp$piece, data_temp$order),]    
title = paste("Percent Change in Attributable Mortality rate due to all risk factors-1990-2016", sep=" ")
        
print(ggplot(data_temp) +
        geom_polygon(aes(x=long, y=lat, group=group, fill=change)) +
        scale_fill_gradientn(colours=colors, limits=ylim)  + 
        geom_path(data=provinces, aes(x=long, y=lat, group=group)) + 
        scale_x_continuous("", breaks=NULL) + 
        scale_y_continuous("", breaks=NULL) + 
        coord_fixed(ratio=1) + 
        guides(fill=guide_colourbar(title="Percent Change", barheight=10)) + 
        theme_bw(base_size=10) +
        labs(title=paste(title, sep="")))
    dev.off()