# Initial formatting of the data

library(icesTAF)
taf.library(icesFO)
library(dplyr)

mkdir("data")

# load species list
species_list <- read.taf("bootstrap/initial/data/FAO_ASFIS_species/species_list.csv")
sid <- read.taf("bootstrap/initial/data/ICES_StockInformation/sid.csv")

# 1: ICES official cath statistics

hist <- read.taf("bootstrap/data/ICES_nominal_catches/ICES_historical_catches.csv")
official <- read.taf("bootstrap/data/ICES_nominal_catches/ICES_2006_2018_catches.csv")
prelim <- read.taf("bootstrap/data/ICES_nominal_catches/ICES_preliminary_catches.csv")

catch_dat <- 
        format_catches(2021, "Azores", 
                       hist, official, prelim, species_list, sid)

update <- read.csv("species_classification_azores.csv")
check <- catch_dat %>% filter(COMMON_NAME %in% update$COMMON_NAME)
check <- check %>% left_join(update, by= "COMMON_NAME")
names(check)
check <- check[,-c(4,11,12)]
names(check)
colnames(check) <- c("YEAR","COUNTRY","ISO3","ECOREGION","SPECIES_NAME","SPECIES_CODE","COMMON_NAME","VALUE", "GUILD")

out <- update$COMMON_NAME
library(operators)
catch_dat <- dplyr::filter(catch_dat, COMMON_NAME %!in% out)
detach("package:operators", unload=TRUE)

catch_dat <- rbind(catch_dat,check)


write.taf(catch_dat, dir = "data", quote = TRUE)

# 2: STECF effort and landings

effort <- read.taf("bootstrap/initial/data/STECF_effort_data.csv", check.names = TRUE)
effort <- read.taf("bootstrap/initial/data/vms_effort_data.csv", check.names = TRUE) #  I am running this cause I don't have the STECF data

landings <- read.taf("bootstrap/initial/data/STECF_landings_data.csv", check.names = TRUE)
landings <- read.taf("bootstrap/initial/data/vms_landings_data.csv", check.names = TRUE)#  I am running this cause I don't have the STECF data

frmt_effort <- format_stecf_effort(effort)
effort <- effort %>% rename('regulated.area' = 'regulated area')
effort <- effort %>% rename('regulated.gear' = 'regulated gear')
frmt_effort <- format_stecf_effort(effort)
frmt_landings <- format_stecf_landings(landings)
landings <- landings %>% rename('regulated.area' = 'regulated area')
landings <- landings %>% rename('regulated.gear' = 'regulated gear')
frmt_landings <- format_stecf_landings(landings)

write.taf(frmt_effort, dir = "data", quote = TRUE)
write.taf(frmt_landings, dir = "data", quote = TRUE)


# 3: SAG
sag_sum <- read.taf("bootstrap/initial/data/SAG_data/SAG_summary.csv")
sag_refpts <- read.taf("bootstrap/initial/data/SAG_data/SAG_refpts.csv")
sag_status <- read.taf("bootstrap/initial/data/SAG_data/SAG_status.csv")

StockList <- c("thr.27.nea",
               "bsf.27.nea",
               "cyo.27.nea",
               "gag.27.nea",
               "gfb.27.nea",
               "guq.27.nea",
               "jaa.27.10a2",
               "alf.27.nea",
               "por.27.nea",
               "raj.27.1012",
               "sbr.27.10",
               "sck.27.nea")


clean_sag <- format_sag(sag_sum, sag_refpts, 2021, "Azores")
clean_status <- format_sag_status(sag_status, 2021, "Azores")
clean_sag<-clean_sag%>%filter(StockKeyLabel %in% StockList)
clean_status<-clean_status%>%filter(StockKeyLabel %in% StockList)                  
write.taf(clean_sag, dir = "data")
write.taf(clean_status, dir = "data", quote = TRUE)
