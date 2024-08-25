# Last run on 8/5/23
# This script can take multiple days to run, depending on the computer, and
#   there are likely to be gages that fail to download the first time around
#   since it's pulling from government servers. Gage ids in the area of
#   interest are found by Googling each state and compiling a list of gage
#   info from the USGS website.
# Existing flood stages were found at:
#   https://waterwatch.usgs.gov/index.php?r=wy&id=flood&sid=w_table
# Corresponding discharge to flood stages were found at:
#   https://waterwatch.usgs.gov/index.php

library(imputeTS)
library(lubridate)
library(data.table)
library(fasttime)
# function taken from rsnodas
source("R/download_usgs_ind.R")
# for download_usgs functionality
library(anytime)
library(httr)
library(dplyr)
library(tidyr)

## retaining hourly data
hv <- function(usgs, timez) {
  # convert to data.table
  data.table::setDT(usgs)

  # enable grouping by individual time specs
  usgs[, year := substr(date, 1, 4)]
  usgs[, month := substr(date, 6, 7)]
  usgs[, day := substr(date, 9, 10)]
  usgs[, hour := substr(time, 1, 2)]

  # calculate hourly maximum flow
  usgs[, max_flow := max(v00060), by = .(year, month, day, hour)]

  # convert from character to datetime object and set tz
  usgs[, hdt := fasttime::fastPOSIXct(paste0(date, " ", hour, ":00:00"),
                                      tz = "GMT"
  ),
  by = .(year, month, day, hour)
  ]
  usgs[, datetime := lubridate::force_tz(hdt, tzone = timez)]

  # save only hourly info
  grusgs <- usgs[, .(
    id = last(site_no),
    max_flow = last(max_flow)
  ),
  by = datetime
  ]
  grusgs
}

## clean data for specific stations
clean_dat <- function(site_no, timez) {
  usgs <- download_usgs(
    freq = "uv",
    destpath = paste0(getwd(), "/data-raw/usgs"),
    sites = as.character(site_no),
    begin_date = as.Date("1980-01-02"),
    end_date = Sys.Date()
  )

  # produce cleaned hourly data
  test <- hv(usgs, timez)

  # HOURLY create vector of dates/ids
  datetime <- seq(test$datetime[1],
                  lubridate::with_tz(Sys.Date(), tzone = timez),
                  by = "hour"
  )
  id <- rep(usgs$site_no[1], times = length(datetime))
  df <- data.frame(id, datetime)

  # produce actual time series
  comp_test <- dplyr::left_join(df, test)
  imp_usgs <- imputeTS::na_interpolation(comp_test, option = "linear")

  imp_usgs
}

## get data frame with all stations
# import station ids
usgs_fs_cl <- data.table::fread("data-raw/usgs_fs/usgs_fs_fin.csv")

# filter for states of interest - already have WY
states <- c("AZ", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WA", "WY")

for (x in seq_along(states)) {
  usgs_abb <- usgs_fs_miss[state == states[x]]
  station_list <- vector("list", length = nrow(usgs_abb))
  vec <- c()
  for (i in seq_along(usgs_abb$site_no)) {
    station_list[[i]] <- tryCatch(
      {
        clean_dat(
          usgs_abb$site_no[i],
          usgs_abb$tz[i]
        )
      },
      error = function(e) e
    )
    if (!inherits(station_list[[i]], "data.frame")) {
      vec[i] <- i
    }
  }

  if (!inherits(vec, "NULL")) {
    station_list2 <- station_list[-vec[which(!is.na(vec))]]
  } else {
    station_list2 <- station_list
  }

  state_mat <- do.call(rbind, station_list2)
  rhv_tot <- as.data.frame(state_mat)
  saveRDS(rhv_tot,
          file = paste0("data-raw/rhv_tot/rhv_tot_", states[x], ".RDS"),
          compress = TRUE
  )
}

# this process is repeated for stations that fail to download the first time
#   and files for each state are saved in the 'rhv_miss' data folder and labeled
#   'rhv_miss' instead of 'rhv_tot'
