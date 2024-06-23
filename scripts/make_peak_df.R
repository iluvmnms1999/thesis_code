## libraries
library(data.table)
library(imputeTS)
library(ggplot2)

## make data matrix --------------------------------------------------------
# peaks data
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
peaks_all <- readRDS("data-raw/ros_class/huc_match/melt_snotel/ge1snotel/add_base_med/ms_baseref_ALL.RDS")
# get date from datetime
peaks_all[, date := as.Date(dt)]
# get dates of all peaks to look at snotel measurements for
dates <- peaks_all[, .(date, huc)]

# snotel data
snotel_all <- readRDS("data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_ALL.RDS")

# take median of snotel conditions for previous five days for each date/huc
data_agg <- data.frame()
for (i in seq_len(nrow(dates))) {
  temp <- snotel_all[huc == dates$huc[i]]
  temp <- temp[date %in% seq(dates$date[i] - 5,
                                   dates$date[i] - 1, by = "day")]
  all_vars <- temp[, .(temp_degc_av = mean(temp_degc, na.rm = TRUE),
                       temp_degc_med = median(temp_degc, na.rm = TRUE),
                       temp_degc_min = min(temp_degc, na.rm = TRUE),
                       temp_degc_max = max(temp_degc, na.rm = TRUE),
                       prec_av = mean(prec, na.rm = TRUE),
                       prec_med = median(prec, na.rm = TRUE),
                       prec_min = min(prec, na.rm = TRUE),
                       prec_max = max(prec, na.rm = TRUE),
                       prec_sum = sum(prec, na.rm = TRUE),
                       snow_dep_av = mean(snow_dep, na.rm = TRUE),
                       snow_dep_med = median(snow_dep, na.rm = TRUE),
                       snow_dep_min = min(snow_dep, na.rm = TRUE),
                       snow_dep_max = max(snow_dep, na.rm = TRUE),
                       swe_av = mean(swe, na.rm = TRUE),
                       swe_med = median(swe, na.rm = TRUE),
                       swe_min = min(swe, na.rm = TRUE),
                       swe_max = max(swe, na.rm = TRUE),
                       soil_mp8in_av = mean(soil_mp8in, na.rm = TRUE),
                       soil_mp8in_med = median(soil_mp8in, na.rm = TRUE),
                       soil_mp8in_min = min(soil_mp8in, na.rm = TRUE),
                       soil_mp8in_max = max(soil_mp8in, na.rm = TRUE),
                       soil_mp20in_av = mean(soil_mp20in, na.rm = TRUE),
                       soil_mp20in_med = median(soil_mp20in, na.rm = TRUE),
                       soil_mp20in_min = min(soil_mp20in, na.rm = TRUE),
                       soil_mp20in_max = max(soil_mp20in, na.rm = TRUE),
                       melt_av = mean(melt, na.rm = TRUE),
                       melt_med = median(melt, na.rm = TRUE),
                       melt_min = min(melt, na.rm = TRUE),
                       melt_max = max(melt, na.rm = TRUE),
                       elev_av = mean(elev, na.rm = TRUE),
                       elev_med = median(elev, na.rm = TRUE),
                       elev_min = min(elev, na.rm = TRUE),
                       elev_max = max(elev, na.rm = TRUE)
                       ), by = id]

  vars_sum <- sapply(all_vars[, -1], median)
  data_agg <- rbind(data_agg, c(as.numeric(dates$date[i]), dates$huc[i], vars_sum))
}

# reset names and convert dates back to dates, replace Inf with NA
names(data_agg) <- c("date", "huc", names(vars_sum))
data_agg$date <- as.Date(data_agg$date)
is.na(data_agg) <- sapply(data_agg, is.infinite)

# save data table
saveRDS(data_agg, "data-raw/modeling/snotel_av_med_FIXED.rds")

# which STATIONS have smp reported for at least part of their period of record?
stat_soilmp <- snotel_all[, .(total = .N,
                              eight_perc = round((.N - sum(is.na(soil_mp8in)))
                                                 / .N, 3),
                              twenty_perc = round((.N - sum(is.na(soil_mp20in)))
                                                  / .N, 3)),
                          by = id]

# which HUCS have smp reported for at least part of their period of record?
huc_soilmp <- snotel_all[, .(total = .N,
                             eight_perc = round((.N - sum(is.na(soil_mp8in)))
                                                / .N, 3),
                             twenty_perc = round((.N - sum(is.na(soil_mp20in)))
                                                 / .N, 3),
                             date = date),
                         by = huc]#[eight == 0 & twenty == 0]

# connect smp findings to peaks
peaks_sel <- peaks_all[, ":="(smp = ifelse(huc %in% huc_soilmp$huc[which(huc_soilmp$eight_perc == 0
                                                                         & huc_soilmp$twenty_perc == 0)],
                                           0, 1),
                              mult = peakflow / base_med)][base_med > 0]

# join snotel and peaks by huc and date
snotel_smp <- readRDS("data-raw/modeling/snotel_av_med_FIXED.rds")

peak_data_dt <- dplyr::left_join(peaks_sel, snotel_smp, by = c("date", "huc"),
                                 multiple = "any")
# final adjustments
peak_data_dt[, ros_num := ifelse(ros == "ros", 1, 0)]

# add lat long to df
usgs <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")
colnames(usgs)[2] <- "id"
peak_data_dt$id <- as.integer(peak_data_dt$id)

peak_data_fin <- dplyr::left_join(peak_data_dt, usgs[, .(id, dec_lat_va, dec_long_va)],
                                  by = dplyr::join_by(id))

# assign each id to random number between 1 and 10 for cv purposes
cv <- data.frame(id = unique(peak_data_fin$id),
                 cv = sample(1:10, length(unique(peak_data_fin$id)),
                             replace = TRUE))

peaks_cv <- dplyr::left_join(peak_data_fin, cv, by = "id")

# add huc 4 var
peaks_cv$huc4 <- substr(peaks_cv$huc, 1, 4)

# make geometries
ws <- readRDS("data-raw/wbd/ws_huc4_geom.rds")
peaks_sf <- sf::st_as_sf(peaks_cv, coords = c("dec_long_va", "dec_lat_va"),
                         crs = sf::st_crs(ws)) |>
  dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                lat = sf::st_coordinates(geometry)[,2])

saveRDS(peaks_sf, "data-raw/modeling/peak_data_sf_FIXED.rds")
