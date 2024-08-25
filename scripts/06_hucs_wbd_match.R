# ~ 3 min to run
# This script matches adds HUCs to streamgage and SNOTEL data to enable spatial
#   association.
# HUC data from the WBD is found at:
#   https://datagateway.nrcs.usda.gov/GDGOrder.aspx?order=QuickState

library(sf)
library(tidyverse)
library(ggmap)

states <- c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa", "wy")
usgs_full <- readRDS("data-raw/usgs_fs/usgs_fs_alm.RDS")
data.table::setDT(usgs_full)
# add empty column for huc to go in
usgs_huc <- usgs_full[, huc8 := rep("NA", length = nrow(usgs_full))]

for (i in seq_along(states)) {
  # read in huc8 shapefile
  temp <- sf::st_read(
    dsn = paste0("data-raw/wbd/", states[i], "_huc8"),
    layer = paste0("wbdhu8_a_", states[i])
  )
  temp1 <- sf::st_make_valid(temp)

  # subset usgs file on specific state
  usgs <- usgs_full[usgs_full$state == toupper(states[i]), ]

  # find which huc usgs station is in
  usgs_sf <- sf::st_as_sf(usgs, coords = c("dec_long_va", "dec_lat_va"),
                          crs = "NAD83", remove = FALSE)
  huc_lst <- sf::st_intersects(usgs_sf, temp1)
  huc_ind <- unlist(huc_lst)
  huc_vec <- temp1[huc_ind, 11]
  usgs_huc[state == toupper(states[i])]$huc8 <- huc_vec$huc8
}
saveRDS(usgs_huc, "data-raw/usgs_fs/usgs_huc.RDS")

## Get hucs for SNOTEL
states <- c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa", "wy")
snotel_full <- read.csv("data-raw/snotel/snotel_id.csv", header = TRUE)
data.table::setDT(snotel_full)
snotel_huc <- snotel_full[, huc8 := rep("NA", length = nrow(snotel_full))]

for (i in seq_along(states)) {
  # read in huc8 shapefile
  temp <- sf::st_read(
    dsn = paste0("data-raw/wbd/", states[i], "_huc8"),
    layer = paste0("wbdhu8_a_", states[i])
  )
  temp1 <- sf::st_make_valid(temp)

  # subset usgs file on specific state
  snotel <- snotel_full[snotel_full$state == toupper(states[i]), ]

  # find which huc usgs station is in
  snotel_sf <- sf::st_as_sf(snotel, coords = c("lon", "lat"),
                            crs = "NAD83", remove = FALSE)
  huc_lst <- sf::st_intersects(snotel_sf, temp1)
  huc_ind <- unlist(huc_lst)
  huc_vec <- temp1[huc_ind, 11]
  snotel_huc[state == toupper(states[i])]$huc8 <- huc_vec$huc8
}
# get rid of snotel with unknown description (58)
snotel_huc1 <- snotel_huc[snotel_huc$state %in% toupper(states), ][-58]

saveRDS(snotel_huc1, "data-raw/snotel/snotel_huc.RDS")
