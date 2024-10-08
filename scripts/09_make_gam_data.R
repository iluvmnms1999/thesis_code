# ~ 10 min to run
# This script creates ROS and non-ROS profiles for each streamgage to be used
#   for representing streamflow surge. Profiles using a halved snow profile are
#   also created.

library(data.table)
library(tidyverse)
library(lubridate)

# av temp: use median of overall distrs from the plot (ros vs non-ros)
# av snow dep: 0 for non-ros, med ann max for ros
# prec_max (or log): 50th percentile of annual max for both non-ros and ros
# av swe: 0 for non-ros, med ann max for ros
# base_med (log): median overall by location

# predict with gam twice for each location using both profiles, compute ratio
# of both predictions and examine distribution of all ratios

# import peak data
peak_data <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
data.table::setDT(peak_data)

# get snotel summaries
snotel_all <- readRDS("data-raw/snotel/snotel_hucmeltelev_ALL.RDS") |>
  mutate(huc = as.numeric(huc),
         n_stat = as.numeric(n_stat),
         elev = as.numeric(elev)) |>
  select("id", "date", "temp_degc", "prec", "snow_dep", "swe", "soil_mp8in",
         "soil_mp20in", "huc", "n_stat", "melt", "elev")

# get summaries using med ann max for ros swe ----------------------------
snotel_summaries <- data.frame()
for (i in seq_along(unique(snotel_all$huc))) {
  temp <- snotel_all[huc == unique(snotel_all$huc)[i]]
  temp[, year := lubridate::year(date)]

  snowdep <- temp[, .(ann_max = max(snow_dep, na.rm = TRUE)),
                  by = year][!is.infinite(ann_max)]
  med_snowdep <- median(snowdep$ann_max, na.rm = TRUE)

  prec_max <- temp[, .(ann_max = max(prec, na.rm = TRUE)),
                   by = year][!is.infinite(ann_max)]
  prec_25 <- quantile(prec_max$ann_max, .25, na.rm = TRUE, names = FALSE)
  prec_75 <- quantile(prec_max$ann_max, .75, na.rm = TRUE, names = FALSE)
  # added for consistent prec across both profiles
  prec_50 <- median(prec_max$ann_max, na.rm = TRUE)

  swe <- temp[, .(ann_max = max(swe, na.rm = TRUE)),
              by = year][!is.infinite(ann_max)]
  med_swe <- median(swe$ann_max, na.rm = TRUE)

  vec <- c(unique(snotel_all$huc)[i], med_snowdep, prec_25, prec_50, prec_75,
           med_swe)

  snotel_summaries <- rbind(snotel_summaries, vec)
}
colnames(snotel_summaries) <- c("huc", "med_snowdep", "prec_25", "prec_50",
                                "prec_75", "med_swe")
data.table::setDT(snotel_summaries)

# baseflow summaries by streamgage
med_bf <- peak_data |>
  group_by(id) |>
  summarise(med_bf = median(base_med),
            huc = huc) |>
  slice(1) |>
  as.data.frame()

var_summaries <- left_join(med_bf, snotel_summaries, join_by(huc))

med_temps <- peak_data[, .(med_temp = median(temp_degc_av, na.rm = TRUE)),
                       by = ros]

setDT(var_summaries)
var_summaries$nonros_temp_med <- med_temps$med_temp[1]
var_summaries$ros_temp_med <- med_temps$med_temp[2]
var_summaries$nonros_snowdep <- 0
var_summaries$nonros_swe <- 0

colnames(var_summaries)[4:8] <- c("ros_snowdep", "nonros_prec", "prec_med",
                                  "ros_prec", "ros_swe")

var_summaries <- var_summaries[, c(3, 1, 2, 11, 4, 5, 7, 6, 12, 8, 9, 10)]

# add lat lon
gam_data <- left_join(var_summaries,
                      peak_data[, c("id", "lat", "lon")],
                      join_by(id),
                      multiple = "any")

test <- gam_data |>
  pivot_longer(
    cols = c(4:7, 9:12),
    names_to = c("ros", "var"),
    names_sep = "_",
    values_to = "val"
  ) |>
  pivot_wider(names_from = var, values_from = val) |>
  data.table::setDT()

saveRDS(test, "data-raw/modeling/gam_datav3.rds")


# get summaries using halved med ann max for ros swe ------------------------
snotel_summaries <- data.frame()
for (i in seq_along(unique(snotel_all$huc))) {
  temp <- snotel_all[huc == unique(snotel_all$huc)[i]]
  temp[, year := data.table::year(date)]

  snowdep <- temp[, .(ann_max = max(snow_dep, na.rm = TRUE)),
                  by = year][!is.infinite(ann_max)]
  med_snowdep <- median(snowdep$ann_max, na.rm = TRUE) / 2

  prec_max <- temp[, .(ann_max = max(prec, na.rm = TRUE)),
                   by = year][!is.infinite(ann_max)]
  prec_25 <- quantile(prec_max$ann_max, .25, na.rm = TRUE, names = FALSE)
  prec_75 <- quantile(prec_max$ann_max, .75, na.rm = TRUE, names = FALSE)
  # added for consistent prec across both profiles
  prec_50 <- median(prec_max$ann_max, na.rm = TRUE)

  swe <- temp[, .(ann_max = max(swe, na.rm = TRUE)),
              by = year][!is.infinite(ann_max)]
  med_swe <- median(swe$ann_max, na.rm = TRUE) / 2

  vec <- c(unique(snotel_all$huc)[i], med_snowdep, prec_25, prec_50, prec_75,
           med_swe)

  snotel_summaries <- rbind(snotel_summaries, vec)
}
colnames(snotel_summaries) <- c("huc", "med_snowdep", "prec_25", "prec_50",
                                "prec_75", "med_swe")
data.table::setDT(snotel_summaries)

# drop geometry from peak_data
peak_data <- sf::st_drop_geometry(peak_data)

# baseflow summaries by streamgage
med_bf <- peak_data |>
  group_by(id) |>
  summarise(med_bf = median(base_med),
            huc = huc) |>
  slice(1) |>
  as.data.frame()

var_summaries <- left_join(med_bf, snotel_summaries, join_by(huc))

med_temps <- peak_data[, .(med_temp = median(temp_degc_av, na.rm = TRUE)),
                       by = ros]

setDT(var_summaries)
var_summaries$nonros_temp_med <- med_temps$med_temp[1]
var_summaries$ros_temp_med <- med_temps$med_temp[2]
var_summaries$nonros_snowdep <- 0
var_summaries$nonros_swe <- 0

colnames(var_summaries)[4:8] <- c("ros_snowdep", "nonros_prec", "prec_med",
                                  "ros_prec", "ros_swe")

var_summaries <- var_summaries[, c(3, 1, 2, 11, 4, 5, 7, 6, 12, 8, 9, 10)]

# add lat lon
gam_data <- left_join(var_summaries,
                      peak_data[, c("id", "lat", "lon")],
                      join_by(id),
                      multiple = "any")

test2 <- gam_data |>
  pivot_longer(
    cols = c(4:7, 9:12),
    names_to = c("ros", "var"),
    names_sep = "_",
    values_to = "val"
  ) |>
  pivot_wider(names_from = var, values_from = val) |>
  data.table::setDT()

saveRDS(test2, "data-raw/modeling/gam_data_swehalved.rds")
