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

### Tables
loc.table <- get_location_metadata(location_set_id = 22)
sdi.table <- get_covariate_estimates(881)[,.(location_id, location_name, year_id, mean_value)]
setnames(sdi.table, "mean_value", "sdi")

# Get Chinese provinces excluding Hong Kong and Macao
prov.list <- setdiff(c(loc.table[parent_id == 6, location_id], loc.table[parent_id == 44533, location_id]), 44533)
remove <- c(361, 354) # HK and Macao
prov.list <- setdiff(prov.list, remove)

## Pull SEVs
risk_ids <- c(86, 87, 83, 84, 240, 241, 94, 136, 137, 238, 334, 335, 100)
sev.dt <- get_outputs(topic = "rei", location_id = prov.list, year_id = c(1990,2016), measure_id = 29, rei_id = risk_ids, metric_id = 3, 
	age_group_id = 1, version = "latest", sex_id = 3, gbd_round_id = 4)
sev.dt <- sev.dt[, .(location_id,year_id,rei_id,rei_name,sex_id,val)]
setnames(sev.dt, "val", "sev")

#Pull pops to transform SEVs in counts
pop.dt <- get_population(location_id = c(prov.list, 44533), year_id = c(1990,2016), age_group_id = 1, sex_id = 3, gbd_round_id = 4)
pop.dt[, c("process_version_map_id", "sex_id") := NULL]
pop.chn <- pop.dt[location_id == 44533,]
pop.chn[, location_name := "China"]

#Merge onto SEVs dataset and aggregate to national
sevagg.dt <- merge(sev.dt, pop.dt, by = c("location_id", "year_id"))
sevagg.dt <- sevagg.dt[, sev_counts := population * sev]
sevagg.dt <- sevagg.dt[,lapply(.SD, sum), by = .(rei_id, rei_name, year_id), .SDcols = "sev_counts"]
sevagg.dt <- merge(sevagg.dt, pop.chn, by = "year_id")
sevagg.dt <- sevagg.dt[, sev := sev_counts/population]
sevagg.dt <- sevagg.dt[, .(location_name, location_id, year_id, sev, rei_name, rei_id)]
sevall.dt <- rbind(sevagg.dt, sev.dt, fill = T)
sevall.dt <- sevall.dt[, .(location_id, year_id, sev, rei_name, rei_id)]

# Merge on sdi to SEV dataframe
plot.dt <- merge(sevall.dt, sdi.table, by = c("location_id", "year_id"))
plot.dt[, corr := cor(sdi,sev), by = .(rei_name, year_id)]
plot.dt[, label := paste0(rei_name, "\n", year_id, "\n", corr)]

#Format so plot looks prettier
plot.dt[rei_name == "Ambient particulate matter pollution", rei_name := "Ambient air pollution"]
plot.dt[rei_name == "Household air pollution from solid fuels", rei_name := "Household air pollution"]
plot.dt[rei_name == "Non-exclusive breastfeeding", rei_name := "Non-exc breastfeeding"]
plot.dt[rei_name == "Discontinued breastfeeding", rei_name := "Disc breastfeeding"]
plot.dt[rei_name == "Short gestation for birth weight", rei_name := "Short gestation"]
plot.dt[rei_name == "Low birth weight for gestation", rei_name := "Low birthweight"]
plot.dt[rei_name == "No access to handwashing facility", rei_name := "No handwashing"]

# Do the ggplot
scatter.path <- paste0(root, "temp/wgodwin/chn/sev/sev_facet_risk_test.pdf")
legend_title <- "Year"
plot.dt$year_id <-as.factor(plot.dt$year_id)
pdf(scatter.path)
gg <- ggplot(plot.dt,
				aes(x=sdi,
					y=sev,
				color=year_id)) +
	geom_point() +
	#stat_smooth() +
	facet_wrap(~rei_name) +
	ggtitle("SEV vs SDI for top Risk Factors") +
	scale_color_discrete(legend_title) +
	labs(x = "Socio-Demographic Index",
	    	y = "Summary Exposure Value")+
  theme_grey()
print(gg)
dev.off()


scatter.path <- paste0(root, "temp/wgodwin/chn/sev/sdi_plot.pdf")
legend_title <- "Location"
#plot.dt$year_id <-as.factor(plot.dt$year_id)
pdf(scatter.path)
gg <- ggplot(chn,
				aes(x=year_id,
					y=sdi,
				color=location_name)) +
	#geom_point() +
	geom_line() +
	#stat_smooth() +
	#facet_wrap(~location_name) +
	ggtitle("SDI for China") +
	scale_color_discrete(legend_title) +
	labs(x = "Time",
	    	y = "Socio-Demographic Index")
print(gg)
dev.off()