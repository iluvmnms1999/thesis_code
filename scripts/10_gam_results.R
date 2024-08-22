library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

# load data matrix
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
# replace NA snow depth with where swe is 0 with 0
ind <- which(peak_data_dt$swe_av == 0 & is.na(peak_data_dt$snow_dep_av))
peak_data_dt$snow_dep_av[ind] <- 0

form_lst <- list(
  log(mult) ~ #1
    s(temp_degc_av) +
    s(temp_degc_med) +
    # s(snow_dep_av) +
    s(prec_av) +
    s(prec_max) +
    s(prec_med) +
    s(prec_sum) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(log(base_med)) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~ #2
    s(temp_degc_av) +
    # s(snow_dep_av) +
    s(prec_av) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(log(base_med)) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~ #3
    s(temp_degc_med) +
    # s(snow_dep_av) +
    s(prec_av) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(log(base_med)) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~ #4
    s(temp_degc_av) +
    # s(snow_dep_av) +
    s(prec_max) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(log(base_med)) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~ #5
    s(temp_degc_med) +
    # s(snow_dep_av) +
    s(prec_max) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(log(base_med)) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~ #6
    s(temp_degc_av) +
    # s(snow_dep_av) +
    s(prec_sum) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(log(base_med)) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~ #7
    s(temp_degc_med) +
    # s(snow_dep_av) +
    s(prec_sum) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(log(base_med)) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~ #8
    s(temp_degc_av) +
    # s(snow_dep_av) +
    s(prec_med) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(log(base_med)) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~ #9
    s(temp_degc_med) +
    # s(snow_dep_av) +
    s(prec_med) +
    # s(melt_av) +
    # s(elev_av) +
    s(swe_av) +
    s(log(base_med)) +
    # smp +
    s(lat, lon, bs = 'sos', k = 25)
)

# 10 fold cross-validation for non-regionalized gam list
gam_nr_mse <- rep(as.numeric(NA), length(form_lst))
gam_nr_mae <- rep(as.numeric(NA), length(form_lst))
for (j in seq_along(form_lst)) {
  gam_nr_preds <- rep(as.numeric(NA), nrow(peak_data_dt))
  for (i in 1:10) {
    index <- peak_data_dt$cv == i

    gam_nr <- mgcv::gam(form_lst[[j]], data = peak_data_dt[!index, ])

    gam_nr_preds[index] <- predict(
      gam_nr, peak_data_dt[index, ]
    )
  }
  gam_nr_capped <- pmax(gam_nr_preds, 0)
  gam_nr_mse[j] <- mean((log(peak_data_dt$mult) - gam_nr_capped)^2, na.rm = TRUE)
  gam_nr_mae[j] <- median(abs(log(peak_data_dt$mult) - gam_nr_capped), na.rm = TRUE)
}

# lowest mse overall:
# log(mult) ~
#   s(temp_degc_av) +
#   s(snow_dep_av) +
#   s(prec_max) +
#   s(swe_av) +
#   s(base_med) +
#   s(lat, lon, bs = 'sos', k = 25)



# visual distr of each important variable ---------------------------------

# facet following plot when ros_num == 1 and ros_num == 0
peak_data_dt %>% # use median of overall distrs from this plot (ros vs non-ros)
  ggplot(aes(x = temp_degc_av, col = ros)) +
  geom_density()

peak_data_dt %>% # 0 for non-ros, med ann max for ros
  ggplot(aes(x = snow_dep_av, col = ros)) +
  geom_density()

peak_data_dt %>% # 25th percentile of annual max for non-ros, 75th for ros
  ggplot(aes(x = prec_max, col = ros)) +
  geom_density()

peak_data_dt %>% # 0 for non-ros, med ann max for ros
  ggplot(aes(x = swe_av, col = ros)) +
  geom_density()

peak_data_dt %>% # median overall by location
  ggplot(aes(x = log(base_med), col = ros)) +
  geom_density()

# predict with gam twice for each location using both profiles, compute ratio
# of both predictions

# check make_gam_data.R

# run gam on each gage ----------------------------------------------------
# define model and train model on profiles
gam_data <- readRDS("data-raw/modeling/gam_datav3.rds")

# replace NA snow depth with 0 when swe is 0
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
ind <- which(peak_data_dt$swe_av == 0 & is.na(peak_data_dt$snow_dep_av))
peak_data_dt$snow_dep_av[ind] <- 0
# peak_data_dt[peak_data_dt$swe_av == 0,] %>%
#   mutate(across(matches("snow_dep"), ~replace(., is.na(.), 0)))

# reformat peak data so we have the same variables
data.table::setDT(peak_data_dt)
peak_data <- peak_data_dt[, .(id, dt, mult, huc, med_bf = base_med, lat, lon,
                              ros,
                              snowdep = snow_dep_av,
                              prec_med = prec_max,
                              swe = swe_av, temp = temp_degc_med)]



# peak_data <- peak_data[sample(c(TRUE, FALSE), size = nrow(peak_data),
#                               prob = c(0.75, 0.25), replace = TRUE), ]

# fit gam
gam_obj <- mgcv::gam(log(mult) ~
                       s(temp) +
                       s(snowdep) +
                       s(prec_med) +
                       s(swe) +
                       s(log(med_bf)) +
                       s(lat, lon, bs = 'sos', k = 25),
                     data = peak_data)

# make preds
preds <- predict(gam_obj, gam_data)
preds_capped <- pmax(preds, 0)

# add preds to predicted data
gam_data$gam_preds <- exp(preds_capped)

# calculate ratios of mults
ratios <- gam_data |> select(id, huc, gam_preds, ros) |>
  group_by(id) |>
  slice(1:2) |>
  pivot_wider(names_from = ros, values_from = gam_preds) |>
  filter(nonros > 1 & ros > 1) |>
  mutate(mult_ratio = ros / nonros)

## PRESENTATION
# visualize ratio of ratios
png("figures/ch4/presentation/total_ratios.png", height = 5, width = 6, units = "in", res = 300)
ratios |>
  ggplot(aes(x = mult_ratio)) +
  geom_density(bw = 0.075) + # smooth it a bit
  geom_vline(aes(xintercept = median(mult_ratio)), col = "red") +
  annotate("text", x = 1.45, y = 0, label = "median = 1.164", color = "red", size = 4) +
  # add mean
  scale_x_continuous(limits = c(0, 2.2), breaks = seq(0, 2.2, 0.2)) +
  ggtitle("Distribution of ROS Stream Surge Ratio") +
  xlab("Ratio") +
  ylab("Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10))
dev.off()

summary(ratios$mult_ratio)


## PAPER
# visualize ratio of ratios
png("figures/ch3/paper/total_ratios.png", height = 5, width = 6, units = "in", res = 300)
ratios |>
  ggplot(aes(x = mult_ratio)) +
  geom_density(bw = 0.075) + # smooth it a bit
  geom_vline(aes(xintercept = median(mult_ratio)), col = "red") +
  annotate("text", x = 1.45, y = 0, label = "median = 1.477", color = "red", size = 4) +
  scale_x_continuous(limits = c(0, 2.2), breaks = seq(0, 2.2, 0.2)) +
  ggtitle("Distribution of ROS Stream Surge Ratio") +
  xlab("Ratio") +
  ylab("Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank())
dev.off()

summary(ratios$mult_ratio)


## PAPER
# regional effects of GAM
png("figures/ch3/paper/gam_effects.png", height = 5, width = 6, units = "in", res = 300)
par(mfrow = c(2, 3))
plot(gam_obj, select = 1,
     xlab = "Temperature", ylab = "s(Temperature, 7.11)",
     xlim = c(-20, 40), ylim = c(-5, 5))
plot(gam_obj, select = 2,
     xlab = "Snow Depth", ylab = "s(Snow Depth, 8.33)",
     xlim = c(0, 400), ylim = c(-2, 2)
     )
plot(gam_obj, select = 3,
     xlab = "Precipitation", ylab = "s(Precipitation, 7.2)",
     xlim = c(0, 250), ylim = c(-1, 3)
)
plot(gam_obj, select = 4,
     xlab = "SWE", ylab = "s(SWE, 3.05)",
     xlim = c(0, 2000), ylim = c(-2, 2)
)
plot(gam_obj, select = 5,
     xlab = "log(Baseflow)", ylab = "s(log(Baseflow), 7.23)",
     xlim = c(-10, 15), ylim = c(-5, 20)
)
dev.off()














# just look at nevada ratios trained on all data
ws <- readRDS("data-raw/wbd/ws_huc8_geom.rds")

# which hucs are in nevada
ind <- which(str_detect(ws$states, "NV"))
nv_hucs <- ws[ind,]$huc8

# get just nevada ratios
nev <- ratios |>
  filter(huc %in% nv_hucs)

# plot
pdf("figures/nev_ratios.pdf", height = 6, width = 6)
nev |>
  ggplot(aes(x = mult_ratio)) +
  geom_density()
dev.off()

summary(nev$mult_ratio)

# just nevada ^^ -------------------------------------------------------------
# get watersheds for all states, combine
# regions <- c("10", "11", "13", "14", "15", "16", "17", "18")
# ws_all <- data.frame()
# for (i in seq_along(regions)) {
#   temp <- sf::st_read(
#     dsn = paste0("data-raw/wbd/huc4_geoms/WBD_", regions[i], "_HU2_Shape/Shape"),
#     layer = "WBDHU8"
#   )
#   temp_val <- sf::st_make_valid(temp)
#   temp_sub <- select(temp_val, c(states, huc8, geometry))
#   ws_all <- rbind(ws_all, temp_sub)
#
#
# }
# # remove duplicates
# ws <- ws_all[!duplicated(ws_all[, c("huc8", "geometry")]),]
# saveRDS(ws, "data-raw/wbd/ws_huc8_geom.rds")
ws <- readRDS("data-raw/wbd/ws_huc8_geom.rds")

# which hucs are in nevada
ind <- which(str_detect(ws$states, "NV"))
nv_hucs <- ws[ind,]$huc8

# # expand bounding boxes of hucs
# bbox_df <- as.data.frame(do.call("rbind", lapply(st_geometry(nv_hucs), st_bbox)))
# bbox_df <- cbind(nv_hucs$huc8, bbox_df)
# head(bbox_df)
#
# # subset left and right nev
# left <- bbox_df |>
#   filter(xmin < -118)

gam_data <- readRDS("data-raw/modeling/gam_data.rds")
data.table::setDT(peak_data_dt)
nv_peaks <- peak_data_dt[huc %in% nv_hucs] # left$`nv_hucs$huc8`
data.table::setDT(nv_peaks)
nv_peak_data <- nv_peaks[, .(id, dt, mult, huc, med_bf = base_med, lat, lon,
                             ros, snowdep = snow_dep_av, prec = prec_max,
                             swe = swe_av, temp = temp_degc_av)]

nv_gam_data <- gam_data[id %in% unique(nv_peak_data$id)]

# fit gam
nv_gam <- mgcv::gam(log(mult) ~
                      s(temp) +
                      s(snowdep) +
                      s(prec) +
                      s(swe) +
                      s(log(med_bf)) +
                      s(lat, lon, bs = 'sos', k = 25),
                    data = nv_peak_data)

# make preds and calc mse and mae
nv_preds <- predict(nv_gam, nv_gam_data)
nv_preds_capped <- pmax(nv_preds, 0)

# add preds to predicted data
nv_gam_data$nv_preds <- nv_preds_capped

# calculate ratios of mults
ratios_nev <- nv_gam_data |> select(id, huc, nv_preds, ros) |>
  group_by(id) |>
  slice(1:2) |>
  pivot_wider(names_from = ros, values_from = nv_preds) |>
  filter(nonros > 1 & ros > 1) |>
  mutate(mult_ratio = ros / nonros)

# visualize ratio of ratios
ratios_nev |>
  ggplot(aes(x = mult_ratio)) +
  geom_density()

summary(ratios_nev$mult_ratio)
