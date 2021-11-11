
library(icesTAF)
library(icesFO)
library(sf)
library(ggplot2)
library(dplyr)


## Run utilies
source("bootstrap/utilities.r")

# set values for automatic naming of files:
cap_year <- 2021
cap_month <- "October"
ecoreg_code <- "AZ"

##########
#Load data
##########

catch_dat <- read.taf("data/catch_dat.csv")


#################################################
##1: ICES nominal catches and historical catches#
#################################################

#~~~~~~~~~~~~~~~#
# By common name
#~~~~~~~~~~~~~~~#
#Plot
plot_catch_trends(catch_dat, type = "COMMON_NAME", line_count = 7, plot_type = "line")
ggplot2::ggsave(file_name(cap_year,ecoreg_code,"Catches_species", ext = "png"), path = "report/", width = 200, height = 130.5, units = "mm", dpi = 300)

#data
dat <- plot_catch_trends(catch_dat, type = "COMMON_NAME", line_count = 7, plot_type = "line", return_data = TRUE)
write.taf(dat, file_name(cap_year,ecoreg_code,"Catches_species", ext = "csv"), dir = "report")


#~~~~~~~~~~~~~~~#
# By country
#~~~~~~~~~~~~~~~#
#Plot
catch_dat$COUNTRY[which(catch_dat$COUNTRY == "Russian Federation")] <- "Russia"
plot_catch_trends(catch_dat, type = "COUNTRY", line_count = 4, plot_type = "area")
ggplot2::ggsave(file_name(cap_year,ecoreg_code,"Catches_country", ext = "png"), path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

#data
dat <- plot_catch_trends(catch_dat, type = "COUNTRY", line_count = 9, plot_type = "area", return_data = TRUE)
write.taf(dat, file= file_name(cap_year,ecoreg_code,"Catches_country", ext = "csv"), dir = "report")

#~~~~~~~~~~~~~~~#
# By guild
#~~~~~~~~~~~~~~~#


# catch_dat2 <- dplyr::filter(catch_dat, GUILD != "Crustacean")
# catch_dat2 <- dplyr::filter(catch_dat2, GUILD != "Elasmobranch")

        #Plot
plot_catch_trends(catch_dat, type = "GUILD", line_count = 4, plot_type = "line")
ggplot2::ggsave(file_name(cap_year,ecoreg_code,"Catches_guild", ext = "png"), path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

        #data
dat <- plot_catch_trends(catch_dat, type = "GUILD", line_count = 4, plot_type = "line", return_data = TRUE)
write.taf(dat, file= file_name(cap_year,ecoreg_code,"Catches_guild", ext = "csv"), dir = "report")

