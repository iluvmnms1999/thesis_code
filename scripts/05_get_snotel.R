# ~ 1.5 hours to run
# This script downloads SNOTEL data from the USGS on a daily scale from 1980-01
#   through 2023-09. Download results can vary since data is being pulled from
#   an online server.
# The list of snotel ids within the area of interest was found at:
#   https://wcc.sc.egov.usda.gov/nwcc/yearcount?network=sntl&state=&counttype=statelist

library(data.table)
library(anytime)
library(httr)
library(dplyr)
library(tidyr)
source("R/download_snotel_ind.R")

snotel_id <- read.csv("data-raw/snotel/snotel_id.csv")
data.table::setDT(snotel_id)
snotel_id[, .N, by = state]

# get number id's for stations
num_id <- stringr::str_extract(snotel_id$site_name, "(\\d+)")

# get vector of all id's to input to download_snotel
site_vec <- c()
for (i in seq_along(snotel_id$site_name)) {
  site_vec[i] <- paste0(num_id[i], ":", snotel_id$state[i], ":SNTL")
}
site_vec

snotel_id <- cbind(site_vec, snotel_id)

# download all stations
states <- c("CO", "ID", "MT", "NM", "NV", "OR", "WY", "WA", "AZ", "UT", "CA")
for (i in seq_along(states)) {
  stations <- snotel_id[snotel_id$state == states[i]]$site_vec
  snotel <- download_snotel(freq = "daily",
                            destpath = paste0("data-raw/snotel/", states[i],
                                              "_snotel"),
                            sites = stations,
                            begin_date = as.Date("1980-01-02"),
                            end_date = as.Date("2023-09-07")
  )
  saveRDS(snotel, paste0("data-raw/snotel/snotel_og_", states[i], ".RDS"))
}

# just keep important variables for snotel
for (i in seq_along(states)) {
  snotel <- readRDS(paste0("data-raw/snotel/snotel_og_", states[i], ".RDS"))
  temp <- snotel[, c("id", "date",
                     "air_temperature_observed_degc_start_of_day_values",
                     "precipitation_increment_mm",
                     "snow_depth_cm_start_of_day_values",
                     "snow_water_equivalent_mm_start_of_day_values",
                     "soil_moisture_percent_8in_pct_start_of_day_values",
                     "soil_moisture_percent_20in_pct_start_of_day_values")]
  names(temp) <- c("id", "date", "temp_degc", "prec", "snow_dep", "swe",
                   "soil_mp8in", "soil_mp20in")
  saveRDS(temp, paste0("data-raw/snotel/snotel_clean_", states[i], ".RDS"))
}
