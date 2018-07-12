## Stacked bar of global child mortality by cause
## Author: Joe Mikesell (Stata); re-written in R by Erika Eldrenkamp
## Date: 04/14/2016; modified 08/09/2017

## SETUP ############################################################
## Bring in libraries
rm(list=ls())
library(foreign)
#library(plyr)
library(data.table)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(Rcpp)
#library("Rcpp", lib.loc="/snfs2/HOME/wgodwin/R/x86_64-unknown-linux-gnu-library/3.1")
#install.packages("ggplot2", type="source",dependencies = TRUE)
library(ggplot2)
library(maptools)
library(RMySQL)

## Setup environment
windows <- Sys.info()[1]=="Windows"
root <- ifelse(Sys.info()[1]=="Windows","J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

## Source in functions
source(paste0(root, "temp/central_comp/libraries/current/r/get_covariate_estimates.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_location_metadata.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_outputs.R"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_population.R"))


## CODE ##############################################################
## Get location metadata
loc.table <- get_location_metadata(location_set_id = 22)

## Generate list of relevant provinces
prov.list <- loc.table[parent_id == 44533, location_id]
locations <- data.frame(prov.list)
setnames(locations, "prov.list", "location_id")

### Generate China mainland estimates (w/o HK & Macau) for mortality rates per 100,000 live births
## Sum province-level outputs to create China mainland output
years <- c(1990, 2016)
risk_ids <- c(86, 87, 83, 84, 240, 241, 94, 136, 137, 238, 334, 335, 100)

# Get outputs of MR due to all causes at province-level
risk.dt <- get_outputs(topic="rei", location_id=prov.list, year_id=years, age_group_id=1, sex_id=3,
	measure_id=1, metric_id=1, rei_id = risk_ids, version="latest", gbd_round_id = 4)
risk.dt[sex=="Male", sex:="Males"]
risk.dt[sex=="Female", sex:="Females"]
risk.dt[sex=="Both", sex:="Both sexes"]
mainland.dt <- copy(risk.dt)
mainland.dt <- mainland.dt[, lapply(.SD, sum), by = .(year_id, sex_id, sex, rei_id, rei_name), .SDcols = "val"]
mainland.dt[, location_id:=44533]
mainland.dt[, location_name:="China"]
deaths.dt <- rbind(risk.dt[, .(location_id, location_name, year_id, sex_id, sex, rei_id, rei_name, val)], mainland.dt)

## Get top 15 causes for each year/sex combo
# Keep top 15 causes for mainland China for each year/sex combo
dtemp.dt <- deaths.dt[location_id==44533]
dtemp10.dt <- dtemp.dt[order(year_id, sex_id, -val), .SD[1:13], by=c("year_id", "sex_id")]
sexes <- unique(dtemp10.dt$sex_id)

# Keep top 15 causes (according to mainland China rates) for each province
risk.dt <- NULL
for(year in years){
	for(sx in sexes) {
		risk.list <- dtemp10.dt[year_id==year & sex_id==sx, rei_id]
		ysdeaths.dt <- deaths.dt[year_id==year & sex_id==sx]

		top10.dt <- subset(ysdeaths.dt, rei_id %in% risk.list)
		risk.dt <- rbind(risk.dt, top10.dt)
	}
}

# Read in live births
pop.dt <- get_population(location_id = c(prov.list, 44533), year_id = years, age_group_id = 1, sex_id = 3, gbd_round_id = 4)

# Merge together and calculate rates
pop.dt <- pop.dt[, .(sex_id, location_id, year_id, population)]
risk.mr <- merge(risk.dt, pop.dt, by=c("location_id", "year_id", "sex_id"))
risk.mr[, rate:=(val/population)*100000]

## GRAPHING #########################################################
## Convential heat map
# Rank causes for each province to be used for heat map color fill
rank <- setorder(risk.mr, year_id, sex_id, location_id, -rate)
n <- length(rank$rei_name)/13
rank[, values:=rep(1:13, times=n)]

# Sum mortality rates to set province order
rank[, mr_sum := sum(rate), by=c("location_id", "year_id", "sex_id")]

# Set desired sexes
sexes <- unique(rank$sex)

# Shortening names
rcod$cause_name[rcod$cause_name == "Hemolytic disease and other neonatal jaundice"] <- "Hemolytic disease and \n other neonatal jaundice"
rcod$cause_name[rcod$cause_name == "Neonatal encephalopathy due to birth asphyxia and trauma"] <- "Neonatal encephalopathy \n due to birth asphyxia \n and trauma"
rcod$cause_name[rcod$cause_name == "Exposure to mechanical forces"] <- "Exposure to mechanical \n forces"
rcod$cause_name[rcod$cause_name == "Neonatal preterm birth complications"] <- "Neonatal preterm birth \n complications"
rcod$cause_name[rcod$cause_name == "Neonatal sepsis and other neonatal infections"] <- "Neonatal sepsis and \n other neonatal infections"
rcod$cause_name[rcod$cause_name == "Other cardiovascular and circulatory diseases"] <- "Other cardiovascular and \n circulatory diseases"
rcod$cause_name[rcod$cause_name == "Paralytic ileus and intestinal obstruction"] <- "Paralytic ileus and intestinal \n obstruction"

# Actually plot
pdf(paste0(root, "temp/", user, "/chn/rf_hmap.pdf",sep=""),width=15,height=9)
rank <- rank[rei_name =="Unsafe water source", rei_name := "Unsafe water"]
rank <- rank[rei_name =="No access to handwashing facility", rei_name := "No handwashing"]
rank <- rank[rei_name =="Disc BF", rei_name := "Disc Breastfeeding"]

for(year in years) {
	#for(sx in sexes) {
		temp <- rank[year_id==year]
		title=paste0("Top under-5 risk factors by province (mortality rate per 100,000) for ", year, ", both sexes")

		print(ggplot(temp, aes(reorder(rei_name, -rate), reorder(location_name, mr_sum))) +
			geom_tile(aes(fill=-values)) +
			geom_text(aes(label=round(rate, 0))) +
			scale_fill_gradient(low="white", high="red", guide=FALSE) +
			labs(title=paste(title, sep=""),
				x="Risk name (ordered by national attributable burden rate, high to low)",
				y="Province (ordered by all-cause attributable mortality rate, high to low)") +
			theme(axis.text.x.top=element_text(angle=45, vjust=0, hjust=0),
				plot.margin=unit(c(1,1,1,1), "cm"),
				plot.title=element_text(hjust=0.5)) +
			scale_x_discrete(position="top"))
	#}
}

dev.off()
all <- data.table()
for(year in c(1995, 2016)){
  for(prov in prov.list){
  t <- fread(paste0("/home/j/temp/wgodwin/chn/bar_chart/new/inputs/figure_2_", prov, "_", year, ".csv"))
  t[, year_id := year]
  t[, location_id := prov]
  all <- rbind(all, t)
  }
}
setnames(all, paste0("val_", 1:14), c("Neonatal preterm birth", "Neonatal encephalopathy due to birth asphyxia and trauma", "Other neonatal disorders", "Lower respiratory infections", "Neonatal sepsis and other neonatal infections", "Diarrhoeal diseases", "Hemolytic disease and other neonatal jaundice", "Meningitis", "Upper respiratory infections", "Sudden infant death syndrome", "Protein-energy malnutrition", "Encephalitis", "Measles", "Otitis media"))
all <- merge(all, loc.table, by = "location_id", all.x = T)
