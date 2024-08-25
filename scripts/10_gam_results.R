# ~ 10 min to run
# This script is used to determine the ideal temp/precip combo for the GAM
#   model and generate stream surge predictions and the ROSSR with the most
#   accurate model.

library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

# define model and train model on profiles -----------------------------------
gam_data <- readRDS("data-raw/modeling/gam_datav3.rds")

# replace NA snow depth with 0 when swe is 0
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
ind <- which(peak_data_dt$swe_av == 0 & is.na(peak_data_dt$snow_dep_av))
peak_data_dt$snow_dep_av[ind] <- 0


# find temp/precip combo with best accuracy -------------------------------
# comment out snowdep for results without snow depth
form_lst <- list(
  log(mult) ~ # model with all temp/precip variables
    s(temp_degc_av) +
    s(temp_degc_med) +
    s(snow_dep_av) +
    s(prec_av) +
    s(prec_max) +
    s(prec_med) +
    s(prec_sum) +
    s(swe_av) +
    s(log(base_med)) +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_av) +
    s(snow_dep_av) +
    s(prec_av) +
    s(swe_av) +
    s(log(base_med)) +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_med) +
    s(snow_dep_av) +
    s(prec_av) +
    s(swe_av) +
    s(log(base_med)) +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_av) +
    s(snow_dep_av) +
    s(prec_max) +
    s(swe_av) +
    s(log(base_med)) +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_med) +
    s(snow_dep_av) +
    s(prec_max) +
    s(swe_av) +
    s(log(base_med)) +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_av) +
    s(snow_dep_av) +
    s(prec_sum) +
    s(swe_av) +
    s(log(base_med)) +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_med) +
    s(snow_dep_av) +
    s(prec_sum) +
    s(swe_av) +
    s(log(base_med)) +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_av) +
    s(snow_dep_av) +
    s(prec_med) +
    s(swe_av) +
    s(log(base_med)) +
    s(lat, lon, bs = 'sos', k = 25),
  log(mult) ~
    s(temp_degc_med) +
    s(snow_dep_av) +
    s(prec_med) +
    s(swe_av) +
    s(log(base_med)) +
    s(lat, lon, bs = 'sos', k = 25)
)

# 10 fold cross-validation for temp/precip combo list
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
  gam_nr_mse[j] <- mean((log(peak_data_dt$mult) - gam_nr_capped)^2,
                        na.rm = TRUE)
  gam_nr_mae[j] <- median(abs(log(peak_data_dt$mult) - gam_nr_capped),
                          na.rm = TRUE)
}
gam_nr_mse
gam_nr_mae


# use best model to generate profile preds --------------------------------
# reformat peak data so we have the same variables as profile data
data.table::setDT(peak_data_dt)
peak_data <- peak_data_dt[, .(id, dt, mult, huc, med_bf = base_med, lat, lon,
                              ros,
                              snowdep = snow_dep_av,
                              prec_med = prec_max,
                              swe = swe_av, temp = temp_degc_med)]

# fit gam
# comment out snowdep to get results without snow depth
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
