################################################################################
## Purpose: Pull results for China under 5 risks paper, then generate figures
## Date created: 08/02/2017
## Date modified:
## Author: Will Godwin, wgodwin@uw.edu
## Run instructions: 
## Notes:
################################################################################

### Setup
rm(list=ls())
windows <- Sys.info()[1]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/risk_factors/")

## Packages
library(data.table)
library(ggplot2)
library(RMySQL)
library(maptools)
library(gridExtra)
library(ggrepel)

### Functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_draws.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_model_results.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_cause_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_population.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_outputs.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_covariate_estimates.R"))
source(paste0(root, "/Project/Mortality/shared/functions/get_age_map.r"))
source(paste0(root, "WORK/05_risk/central/code/maps/global_map.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/make_aggregates.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_rei_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_demographics.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_ids.R"))
r <- get_ids("rei")

### Tables
loc.table <- get_location_metadata(location_set_id = 22)
loc.name <- loc.table[, .(location_id, location_name)]
age.table <- data.table(get_age_map(type = "all"))
meta <- get_cause_metadata(cause_set_id = 2, gbd_round_id = 4)
sdi.table <- get_covariate_estimates(881)[,.(location_id, year_id, mean_value)]
setnames(sdi.table, "mean_value", "sdi")
rei.meta <- get_rei_metadata(rei_set_id = 1)
rei.table <- rei.meta[, c("rei_name", "rei_id")]
demo <- get_demographics(gbd_team="epi", gbd_round_id = 4)

### Code
# Get Chinese provinces excluding Hong Kong and Macao
prov.list <- setdiff(c(loc.table[parent_id == 6, location_id], loc.table[parent_id == 44533, location_id]), 44533)
remove <- c(361, 354) # HK and Macao
prov.list <- setdiff(prov.list, remove)

############################################
###################### SEVs ################
############################################

# Get outputs of SEV's for risks of interest (WaSH, malnutrition, HAP, breastfeeding)
#risk_ids <- c(86, 87, 83, 84, 240, 241, 94, 136, 137, 238, 339, 100, 96, 97)
risk_ids <- c(86, 87, 83, 84, 240, 241, 94, 136, 137, 238, 334, 335, 100)
sev.dt <- get_outputs(topic = "rei", location_id = prov.list, year_id = c(1990,2016), measure_id = 29, rei_id = risk_ids, metric_id = 3, 
	age_group_id = 1, version = "latest", gbd_round_id = 4, sex_id = 3)
setnames(sev.dt, "val", "sev")
sev.dt <- merge(sev.dt, sdi.table, by = c("location_id", "year_id"))

## Loop through risks and scatter the sdi vs sev
scatter.path <- paste0(root, "temp/wgodwin/chn/sev_sdi_scatter.pdf")
pdf(scatter.path)
for(risk in risk_ids) {
	for(year in c(1990,2016)) {
		risk.dt <- sev.dt[rei_id == risk & year_id == year]
		rei_name <- unique(risk.dt$rei_name)
		gg <- ggplot(risk.dt, 
					aes(x = sdi, 
						y = sev)) + 
			geom_point() +
	    	ggtitle(paste0("SEV vs SDI: ", rei_name, " in ", year)) +
	    	geom_text_repel(aes(label = location_name.x)) +
	    	labs(x = "Socio-Demographic Index", 
	    		y = "Summary Exposure Value")
	    print(gg)
	    print(paste(rei_name, year))
	}
}
dev.off()

#################################################################
# SEV OVER TIME SCATTER for top 10 risks
#################################################################
#risk_ids <- c(86, 87, 339, 239, 100, 93, 83, 84, 238, 341)
risk_ids <- c(86, 87, 83, 84, 240, 241, 94, 136, 137, 238, 334, 335, 100)
years <- c(seq(1990, 2010, 5), 2016)
sev.dt <- get_outputs(topic = "rei", location_id = prov.list, year_id = years, measure_id = 29, rei_id = risk_ids, metric_id = 3, 
	age_group_id = 1, version = "latest", sex_id = c(3),gbd_round_id = 4)
setnames(sev.dt, "val", "sev")
sev.dt <- sev.dt[, .(location_id, sex_id, year_id, sev, rei_name, rei_id, location_name)]

#Pull pops to transform SEVs in counts
pop.dt <- get_population(location_id = c(prov.list, 44533), year_id = years, age_group_id = 1, sex_id = -1)
pop.dt[, process_version_map_id := NULL]
pop.chn <- pop.dt[location_id == 44533,]
pop.chn[, location_name := "China"]

#Merge onto SEVs dataset and aggregate to national
sevagg.dt <- merge(sev.dt, pop.dt, by = c("location_id", "year_id", "sex_id"))
sevagg.dt <- sevagg.dt[, sev_counts := population * sev]
sevagg.dt <- sevagg.dt[,lapply(.SD, sum), by = .(rei_id, sex_id, rei_name, year_id), .SDcols = "sev_counts"]
sevagg.dt <- merge(sevagg.dt, pop.chn, by = c("year_id", "sex_id"))
sevagg.dt <- sevagg.dt[, sev := sev_counts/population]
sevagg.dt <- sevagg.dt[, .(location_name, location_id, sex_id, year_id, sev, rei_name, rei_id)]
sevboth.dt <- rbind(sevagg.dt, sev.dt)

# loop through locations and scatter sev's over time
sexes <- unique(sev.dt$sex_id)
scatter.path <- paste0(root, "temp/wgodwin/chn/sev_time_scatter_nat2.pdf")
pdf(scatter.path)
legend_title <- "Risk Name"
for(loc in c(44533, prov.list)) {
	for(sx in sexes) {
		temp.dt <- sevboth.dt[location_id == loc & sex_id == sx,]
		loc_name <- unique(temp.dt$location_name)
		sex_name <- unique(temp.dt$sex)
		gg <- ggplot(temp.dt, 
			aes(x = year_id, 
				y = sev, 
				color = rei_name,
				group = rei_name)) +
			geom_line(data = temp.dt[!is.na(temp.dt$sev),]) + 
			geom_point() + 
			ggtitle(paste0(loc_name)) +
		    	labs(x = "Year", 
		    		y = "Summary Exposure Value (proportion exposed)") +
			scale_color_discrete(legend_title) +
		  theme_classic()
			#facet_wrap(~location_name, ncol = 2, nrow = 2)
		print(gg)
		print(paste(loc, sex_name))
	}
}
dev.off()

##########################################################################
####### Make SEV vs DALYs plot like Figure 4 in GBD RF 2015 capstone paper
##########################################################################
#reis <- rei.meta[level == 3, "rei_id"]
risk_ids <- c(86, 87, 83, 84, 240, 241, 94, 136, 137, 238, 334, 335, 100) ##Correct ones
yr_start <- 1990
yr_end <- 2016
sev.dt <- get_outputs(topic = "rei", location_id = prov.list, year_id = c(yr_start,yr_end), measure_id = 29, rei_id = risk_ids, metric_id = 3, 
	age_group_id = 1, version = "latest", gbd_round_id = 4, sex_id = 3)
setnames(sev.dt, "val", "sev")
sev.dt <- sev.dt[, .(location_id, year_id, sev, rei_name, rei_id, location_name)]

#Pull pops to transform SEVs in counts
pop.dt <- get_population(location_id = c(prov.list, 44533), year_id = c(yr_start,yr_end), age_group_id = 1, sex_id = 3, gbd_round_id = 4)
pop.dt[, process_version_map_id := NULL]
pop.chn <- pop.dt[location_id == 44533,]
pop.chn[, location_name := "China"]

#Merge onto SEVs dataset and aggregate to national
sevagg.dt <- merge(sev.dt, pop.dt, by = c("location_id", "year_id"))
sevagg.dt <- sevagg.dt[, sev_counts := population * sev]
sevagg.dt <- sevagg.dt[,lapply(.SD, sum), by = .(rei_id, rei_name, year_id), .SDcols = "sev_counts"]
sevagg.dt <- merge(sevagg.dt, pop.chn, by = c("year_id"))
sevagg.dt <- sevagg.dt[, sev := sev_counts/population]
sevagg.dt <- sevagg.dt[, .(location_name, location_id, year_id, sev, rei_name, rei_id)]
sevboth.dt <- rbind(sevagg.dt, sev.dt)

## Reshape for calculation
  df <- dcast(sevboth.dt, location_id + rei_id + rei_name + location_name ~ year_id, 
              value.var = "sev")
  setnames(df, c("1990", "2016"), c("old", "new"))
##Calculate rate of change
df <- df[,pct.change:=(((new - old)/ old) * 100) / (yr_end - yr_start)]

## Pull total death counts for each risk and sum to mainland china
mort.dt <- get_outputs(topic = "rei", location_id = prov.list, year_id = yr_end, measure_id = 1, rei_id = risk_ids, metric_id = 1, 
	age_group_id = 1, version = "latest", sex_id = 3, gbd_round_id = 4)
mort.dt <- mort.dt[, .(rei_id, rei_name, location_id, val)]
mort.dt <- mort.dt[,lapply(.SD, sum), by = .(rei_id, rei_name), .SDcols = "val"]
mort.dt[, location_name := "China"]
mort.dt[, location_id := 44533]

#Merge with SEVs and prep for plotting
plot.dt <- merge(df, mort.dt, by=c("rei_name", "location_name"))
plot.dt <- plot.dt[val<.01, val := NA] # Exclude occupational risks with miniscule burden
plot.dt <- plot.dt[abs(pct.change) > 3, pct.change := NA] # exclude BMI b/c too large change
scatter.path <- paste0(root, "temp/wgodwin/chn/sev_pct_change.pdf")
pdf(scatter.path)
for(sex in 1:2){
	lim <- min(plot.dt$pct.change, na.rm = TRUE)
	sex.name <- ifelse(sex == 1, "Males", "Females")
	gg <- ggplot(plot.dt, 
					aes(x = val, 
						y = pct.change)) +
				scale_x_continuous(trans='log10') +
				geom_point() +
		    	coord_cartesian(ylim = c(lim, -lim)) +
		    	ggtitle(paste0("Change in SEV and Total Attributable Under-5 deaths in China")) +
		    	geom_text_repel(aes(label = rei_name)) +
		    	labs(x = "Total Attributable Deaths in 2016", 
		    		y = "Annualized rate of change in SEVs, 1990-2016")
		    print(gg)
}
dev.off()

#################################################
##### All-cause Attributable mortality #####
#################################################
# Get outputs of mortality rate due to risks of interest for provinces
risk_ids <- c(86, 87, 339, 239, 100, 93, 83, 84, 238, 341)
years <- c(seq(1990, 2010, 5), 2016)
mort.dt <-	get_outputs(topic = "rei", location_id = prov.list, year_id = years, measure_id = 1, rei_id = risk_ids, metric_id = 1, 
			age_group_id = 1, version = "latest", sex_id = c(1,2))
mainland.dt <- copy(mort.dt)
mainland.dt <- mainland.dt[, lapply(.SD, sum), by = .(year_id, sex_id, rei_id, age_group_id), .SDcols = c("val", "upper", "lower")]
mainland.dt[, location_id := 44533]
deaths.dt <- rbind(mort.dt[, .(location_id, year_id, sex_id, age_group_id, rei_id, val, upper, lower)], mainland.dt)

# Read in population
pop.dt <- get_population(location_id = c(prov.list, 44533), year_id = years, age_group_id = 1, sex_id = -1)
pop.dt[, process_version_map_id := NULL]

# Merge together and calculate rates
merge.dt <- merge(deaths.dt, pop.dt, by = c("location_id", "year_id", "sex_id", "age_group_id"))
merge.dt[, c("rate", "upper", "lower") := .(val / population * 1e5, upper / population * 1e5, lower / population * 1e5)]
merge.dt <- merge(merge.dt, rei.table, by = "rei_id")
merge.dt <- merge(merge.dt, loc.name, by = "location_id")

## Loop through risks and scatter mort rate across time for both sexes
scatter.path <- paste0(root, "temp/wgodwin/chn/mort_scatter_prov_nat2.pdf")
pdf(scatter.path)
legend_title <- "Risk Name"
for(loc in c(44533, prov.list)) {
	for(sx in 1:2) {
		temp.dt <- merge.dt[location_id == loc & sex_id == sx,]
		sex.name <- ifelse(sx == 1, "Males", "Females")
		loc_name <- unique(temp.dt$location_name)
		gg <- ggplot(temp.dt, 
			aes(x = year_id, 
				y = rate, 
				color = rei_name,
				group = rei_name)) +
			geom_line(data = temp.dt[!is.na(temp.dt$rate),]) + 
			geom_point() + 
			ggtitle(paste0("All-cause mortality by Risk factor in ", loc_name, "-", sex.name)) +
			labs(x = "Year", 
				 y = "Attributable Mortality Rate (per 100,000)") +
			#scale_color_discrete(legend_title) +
			#geom_errorbar(aes(ymin=lower, ymax=upper)) +
			scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#29dd22","#e6ab02","#a6761d","#666666", "#0000EE", "#BB0000"))
			#facet_wrap(~location_name, ncol = 2, nrow = 2)
		print(gg)
		print(paste(loc, sex.name))
	}
}
dev.off()
