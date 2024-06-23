#### classify ROS peaks
library(data.table)
library(dplyr)

#### import/prep necessary data ####
usgs_huc <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")
snotel_huc <- readRDS("data-raw/snotel/snotel_huc.RDS") |>
  mutate(num = stringr::str_extract(site_name, "(\\d+)"))

base_med <- readRDS("data-raw/peaks_fin/peaks_base_med_ref.RDS")
peaks <- readRDS("data-raw/peaks_fin/peaks_tot.RDS")

# add hucs to peaks
peaks <- peaks[, huc := rep(0, nrow(peaks))]
for (i in seq_along(usgs_huc$site_no)) {
  # add huc variable to peak data
  peaks[peaks$id == formatC(usgs_huc$site_no[i],
                            width = 8,
                            format = "d",
                            flag = "0")]$huc <- rep(usgs_huc$huc8[i],
                                                    times = nrow(peaks[peaks$id == formatC(usgs_huc$site_no[i],
                                                                                           width = 8,
                                                                                           format = "d",
                                                                                           flag = "0")]))
}

# subset peaks to just include those in the same hucs as snotel stations
peaks_match <- peaks[peaks$huc %in% as.numeric(snotel_huc$huc8)]

## add hucs and melt to snotel data for each station (TAKES FOREVER)
states <- toupper(c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa",
                    "wy"))
# get all snotel data
snotel <- data.frame()
for (i in seq_along(states)) {
  x <- readRDS(paste0("data-raw/snotel/snotel_clean_", states[i], ".RDS"))
  snotel <- rbind(snotel, x)
}
snotel <- snotel |>
  mutate(num = stringr::str_extract(id, "(\\d+)"))

# filter snotel_huc so it only has the id's from snotel
snotel_huc <- snotel_huc |>
  filter(num %in% unique(snotel$num))

# get rid of NA date rows
snotel <- snotel[!is.na(date)]
# add empty huc var
# snotel <- snotel[, huc := rep(0, nrow(snotel))]
# snotel <- snotel[, n_stat := rep(0, nrow(snotel))]

# add huc variable and elev
snotel <- left_join(snotel, snotel_huc[, c(8, 11, 12)], by = join_by(num))

# add n_stat variable to snotel
counts <- snotel_huc[, length(unique(site_name)), by = huc8]
snotel <- left_join(snotel, counts, by = join_by(huc8))

# fix names
names(snotel)[c(11, 12)] <- c("huc", "n_stat")

# add melt
snotel_melt <- snotel[, melt := swe + prec - shift(swe, type = "lead"),
                      by = id]

# make negative melt values 0
snotel_melt$melt[which(snotel_melt$melt < 0)] <- 0
saveRDS(snotel_melt, "data-raw/snotel/huc_melt_elev/snotel_hucmeltelev_ALL.RDS")

#### SNOTEL: Melt-based ####
## get melt using snotel prec
states <- toupper(c("az", "ca", "co", "id", "mt", "nm", "nv", "or", "ut", "wa",
                    "wy"))

ros_melt_snotel <- snotel[swe >= 10 & prec >= 10 & melt/(melt + prec) >= 0.2]

# add info (variable) for how many of total snotels agree on ros days and only
# retain hucs where there are more than two snotel stations
ros_days <- ros_melt_snotel %>% dplyr::group_by(date, huc) %>%
  dplyr::reframe(l_date = length(date), n_stat = n_stat) %>%
  filter(n_stat >= 1)
intervals <- vector("list", length = length(ros_days$date))
for (y in seq_along(ros_days$date)) {
  int <- seq(as.POSIXct(ros_days$date[y], tz = "US/Pacific"),
             by = "hour", length.out = 168)
  intervals[[y]] <- int
}
ros_days$date_ints <- intervals
super_ros <- ros_days[ros_days$l_date / ros_days$n_stat >= 0.5, ]

peaks_sub <- peaks_match[peaks_match$huc %in% super_ros$huc]
peaks_sub <- peaks_sub[, ros := rep(0, nrow(peaks_sub))]
for (z in seq_along(peaks_sub$y)) {
  temp <- super_ros[super_ros$huc == peaks_sub$huc[z],]
  peaks_sub$ros[z] <- ifelse(as.POSIXct(peaks_sub$dt[z],
                                        format = "%Y-%m-%d",
                                        tz = "US/Pacific") %in%
                               as.POSIXct(unlist(temp$date_ints),
                                          format = "%Y-%m-%d",
                                          tz = "US/Pacific",
                                          origin = "1970-01-01"),
                             "ros", "non-ros")
}

file <- peaks_sub
# add baseflow to df
x <- dplyr::left_join(file, base_med, by = c("state", "id", "dt"))
# get rid of duplicate peakflow
temp <- x[, -7]
names(temp)[4] <- "peakflow"
temp <- temp[, c(1, 2, 5, 3, 4, 7, 6)]
temp
saveRDS(temp, "data-raw/ros_class/huc_match/melt_snotel/ge1snotel/add_base_med/ms_baseref_ALL.RDS")








