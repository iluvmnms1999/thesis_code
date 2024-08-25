# ~ 20 minutes to run
# This script is use to produce ML results using the stressor package for the
#   for comparison with results from the GAM.

library(stressor)
library(reticulate)
library(data.table)
library(dplyr)
library(tidyverse)

# set up environment for stressor
use_python("/Users/emmawatts/anaconda3/bin/python") # replace with own path
stressor::create_virtualenv()
reticulate::use_virtualenv("stressor-env2024-06-24075127184412")

# import peak data and prep for modeling
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
ind <- which(peak_data_dt$swe_av == 0 & is.na(peak_data_dt$snow_dep_av))
peak_data_dt$snow_dep_av[ind] <- 0

data.table::setDT(peak_data_dt)
peak_data_dt <- peak_data_dt[complete.cases(id, dt, mult, huc,
                                            base_med, lat, lon,
                                            ros, snow_dep_av, prec_max,
                                            swe_av, temp_degc_med)
][, .(id, dt, mult, huc,
      base_med, lat, lon,
      ros, snow_dep_av, prec_max,
      swe_av, temp_degc_med, cv)]

# 10 fold cross-validation for condensed formula with lat/lon --------------
preds <- as.data.frame(matrix(0, nrow = nrow(peak_data_dt), ncol = 18))
names(preds) <- c("dt", "rf", "ada", "gbr", "lightgbm", "et", "knn", "en",
                  "ridge", "lasso", "llar", "lar", "lr", "br", "omp", "huber",
                  "par", "dummy")

for (i in 1:10) {
  ind <- peak_data_dt$cv == i
  train <- peak_data_dt[!ind, ]
  test <- peak_data_dt[ind, ]

  mlm_peaks <- mlm_regressor(formula = log(mult) ~
                               temp_degc_med +
                               snow_dep_av +
                               prec_max +
                               swe_av +
                               log(base_med) +
                               lat +
                               lon,
                             train_data = train, seed = 123)

  preds[ind, ] <- mlm_refit(mlm_object = mlm_peaks, train_data = train,
                            test_data = test)
}

# calculate mse with observed log(surge) for each model column in preds
results_base <- sapply(preds, function(x) mean((log(peak_data_dt$mult) - x) ^ 2,
                                               na.rm = TRUE))

# 10 fold cross-validation for condensed formula without lat/lon ------------
preds2 <- as.data.frame(matrix(0, nrow = nrow(peak_data_dt), ncol = 18))
names(preds2) <- c("dt", "rf", "ada", "gbr", "lightgbm", "et", "knn", "en",
                   "ridge", "lasso", "llar", "lar", "lr", "br", "omp", "huber",
                   "par", "dummy")

for (i in 1:10) {
  ind <- peak_data_dt$cv == i
  train <- peak_data_dt[!ind, ]
  test <- peak_data_dt[ind, ]

  mlm_peaks <- mlm_regressor(formula = log(mult) ~
                               temp_degc_med +
                               snow_dep_av +
                               prec_max +
                               swe_av +
                               log(base_med),
                             train_data = train, seed = 123)

  preds2[ind, ] <- mlm_refit(mlm_object = mlm_peaks, train_data = train,
                             test_data = test)
}

# calculate mse with observed log(surge) for each model column in preds
results_base2 <- sapply(preds2,
                        function(x) mean((log(peak_data_dt$mult) - x) ^ 2,
                                                 na.rm = TRUE))


# 10 fold CV for formula with all available variables (except smp) ---------
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
ind <- which(peak_data_dt$swe_av == 0 & is.na(peak_data_dt$snow_dep_av))
peak_data_dt$snow_dep_av[ind] <- 0

data.table::setDT(peak_data_dt)
peak_data_dt <- peak_data_dt[complete.cases(id, dt, mult, huc,
                                            base_med, lat, lon, ros_num,
                                            ros, snow_dep_av, snow_dep_med,
                                            snow_dep_min, snow_dep_max,
                                            prec_max, prec_av, prec_min,
                                            prec_sum, prec_med, swe_av, swe_med,
                                            swe_min, swe_max, temp_degc_med,
                                            temp_degc_av, temp_degc_min,
                                            temp_degc_max, melt_av, melt_med,
                                            melt_min, melt_max, elev_av,
                                            elev_med, elev_min, elev_max,
                                            cv)
][, .(id, dt, mult, huc,
      base_med, lat, lon, ros_num,
      ros, snow_dep_av, snow_dep_med, snow_dep_min, snow_dep_max, prec_max,
      prec_av, prec_min, prec_sum, prec_med, swe_av, swe_med, swe_min, swe_max,
      temp_degc_med, temp_degc_av, temp_degc_min, temp_degc_max, melt_av,
      melt_med, melt_min, melt_max, elev_av, elev_med, elev_min, elev_max,
      cv)]

preds3 <- as.data.frame(matrix(0, nrow = nrow(peak_data_dt), ncol = 18))
names(preds3) <- c("dt", "rf", "ada", "gbr", "lightgbm", "et", "knn", "en",
                   "ridge", "lasso", "llar", "lar", "lr", "br", "omp", "huber",
                   "par", "dummy")

for (i in 1:10) {
  ind <- peak_data_dt$cv == i
  train <- peak_data_dt[!ind, ]
  test <- peak_data_dt[ind, ]

  mlm_peaks <- mlm_regressor(formula = log(mult) ~
                               temp_degc_med +
                               temp_degc_av +
                               temp_degc_min +
                               temp_degc_max +
                               snow_dep_av +
                               snow_dep_med +
                               snow_dep_min +
                               snow_dep_max +
                               prec_av +
                               prec_med +
                               prec_min +
                               prec_max +
                               prec_sum +
                               swe_av +
                               swe_med +
                               swe_min +
                               swe_max +
                               melt_av +
                               melt_med +
                               melt_min +
                               melt_max +
                               elev_av +
                               elev_med +
                               elev_min +
                               elev_max +
                               log(base_med) +
                               ros_num +
                               lon +
                               lat,
                             train_data = train, seed = 123)

  preds3[ind, ] <- mlm_refit(mlm_object = mlm_peaks, train_data = train,
                             test_data = test)
}

# calculate mse with observed log(surge) for each model column in preds
results_base3 <- sapply(preds3,
                        function(x) mean((log(peak_data_dt$mult) - x) ^ 2,
                                                 na.rm = TRUE))


# 10 fold CV for formula with all variables (except smp and lat/lon) ---------
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
ind <- which(peak_data_dt$swe_av == 0 & is.na(peak_data_dt$snow_dep_av))
peak_data_dt$snow_dep_av[ind] <- 0

data.table::setDT(peak_data_dt)
peak_data_dt <- peak_data_dt[complete.cases(id, dt, mult, huc,
                                            base_med, ros_num,
                                            ros, snow_dep_av, snow_dep_med,
                                            snow_dep_min, snow_dep_max,
                                            prec_max, prec_av, prec_min,
                                            prec_sum, prec_med, swe_av, swe_med,
                                            swe_min, swe_max, temp_degc_med,
                                            temp_degc_av, temp_degc_min,
                                            temp_degc_max, melt_av, melt_med,
                                            melt_min, melt_max, elev_av,
                                            elev_med, elev_min, elev_max,
                                            cv)
][, .(id, dt, mult, huc,
      base_med, ros_num,
      ros, snow_dep_av, snow_dep_med, snow_dep_min, snow_dep_max, prec_max,
      prec_av, prec_min, prec_sum, prec_med, swe_av, swe_med, swe_min, swe_max,
      temp_degc_med, temp_degc_av, temp_degc_min, temp_degc_max, melt_av,
      melt_med, melt_min, melt_max, elev_av, elev_med, elev_min, elev_max,
      cv)]

preds4 <- as.data.frame(matrix(0, nrow = nrow(peak_data_dt), ncol = 18))
names(preds4) <- c("dt", "rf", "ada", "gbr", "lightgbm", "et", "knn", "en",
                   "ridge", "lasso", "llar", "lar", "lr", "br", "omp", "huber",
                   "par", "dummy")

for (i in 1:10) {
  ind <- peak_data_dt$cv == i
  train <- peak_data_dt[!ind, ]
  test <- peak_data_dt[ind, ]

  mlm_peaks <- mlm_regressor(formula = log(mult) ~
                               temp_degc_med +
                               temp_degc_av +
                               temp_degc_min +
                               temp_degc_max +
                               snow_dep_av +
                               snow_dep_med +
                               snow_dep_min +
                               snow_dep_max +
                               prec_av +
                               prec_med +
                               prec_min +
                               prec_max +
                               prec_sum +
                               swe_av +
                               swe_med +
                               swe_min +
                               swe_max +
                               melt_av +
                               melt_med +
                               melt_min +
                               melt_max +
                               elev_av +
                               elev_med +
                               elev_min +
                               elev_max +
                               log(base_med) +
                               ros_num,
                             train_data = train, seed = 123)

  preds4[ind, ] <- mlm_refit(mlm_object = mlm_peaks, train_data = train,
                             test_data = test)
}

# calculate mse with observed log(surge) for each model column in preds
results_base4 <- sapply(preds4,
                        function(x) mean((log(peak_data_dt$mult) - x) ^ 2,
                                                 na.rm = TRUE))



