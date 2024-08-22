library(stressor)
library(reticulate)
library(data.table)
library(dplyr)
library(tidyverse)

use_python("/Users/emmawatts/anaconda3/bin/python")

stressor::create_virtualenv()

## remove old virtual envs
# reticulate::virtualenv_list()
# reticulate::virtualenv_remove("stressor-env2022-10-28155903")

# use sam's readme
# try on 1000 peaks and make sure it actually works before applying to whole
#   dataset -- should literally be three lines of code
# can you get it to run? if so, use different CV
reticulate::use_virtualenv("stressor-env2024-06-24075127184412")

peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
ind <- which(peak_data_dt$swe_av == 0 & is.na(peak_data_dt$snow_dep_av))
peak_data_dt$snow_dep_av[ind] <- 0

set.seed(3243)
ind <- sample(1:10, 3, replace = FALSE)
data.table::setDT(peak_data_dt)
train <- peak_data_dt[!(peak_data_dt$cv %in% ind),
                       ][complete.cases(id, dt, mult, huc,
                           base_med, lat, lon,
                           ros, snow_dep_av, prec_max,
                           swe_av, temp_degc_med)
                         ][, .(id, dt, mult, huc,
                               base_med, lat, lon,
                               ros, snow_dep_av, prec_max,
                               swe_av, temp_degc_med, cv)]

test <- peak_data_dt[peak_data_dt$cv %in% ind,
                     ][complete.cases(id, dt, mult, huc,
                          base_med, lat, lon,
                          ros, snow_dep_av, prec_max,
                          swe_av, temp_degc_med)
                       ][, .(id, dt, mult, huc,
                             base_med, lat, lon,
                             ros, snow_dep_av, prec_max,
                             swe_av, temp_degc_med, cv)]

mlm_peaks <- mlm_regressor(formula = log(mult) ~
                             temp_degc_med +
                             snow_dep_av +
                             prec_max +
                             swe_av +
                             log(base_med) +
                             lat +
                             lon,
                           train_data = train, seed = 123)

test_pred <- mlm_refit(mlm_object = mlm_peaks, train_data = train,
                       test_data = test)

results <- stressor::score(log(test$mult), test_pred, metrics = "MSE")

# do train test split (70-30) on cv variable in dataset -- show results for that
# cv on whole dataset and use hyperparameters found from train test split (assume
#   hyperparameters are fixed)
# try to use mlm_refit for cv -- figure out how to input groups
# will take a while to run -- use Sam's paper for guidance

# grouping formula is not applied to data, so idk how to keep the cv groups i
# assigned to dataset
# cv_models <- cv(mlm_peaks, data = peak_data, n_folds = 10, grouping_formula = ?)
# head(score(boston$cmedv, cv_models, metrics = "RMSE"))

data.table::setDT(peak_data_dt)
peak_data_dt <- peak_data_dt[complete.cases(id, dt, mult, huc,
                                            base_med, lat, lon,
                                            ros, snow_dep_av, prec_max,
                                            swe_av, temp_degc_med)
                              ][, .(id, dt, mult, huc,
                                base_med, lat, lon,
                                ros, snow_dep_av, prec_max,
                                swe_av, temp_degc_med, cv)]

# 10 fold cross-validation for formula with lat/lon --------
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

# find ROSSR distr
# read in GAM data
gam_data <- readRDS("data-raw/modeling/gam_datav3.rds")
gam_surge <- gam_data |>
  mutate(mult = 0)

# reformat peak data so we have the same variables
data.table::setDT(peak_data_dt)
peak_data <- peak_data_dt[, .(id, dt, mult, huc, med_bf = base_med, lat, lon,
                              ros,
                              snowdep = snow_dep_av,
                              prec_med = prec_max,
                              swe = swe_av, temp = temp_degc_med)]

# make preds
mlm_peaks <- mlm_regressor(formula = log(mult) ~
                             temp +
                             snowdep +
                             prec_med +
                             swe +
                             log(med_bf) +
                             lat +
                             lon,
                           train_data = peak_data, seed = 123)

preds <- predict(mlm_peaks, gam_surge, train_data = peak_data)
preds_capped <- pmax(preds, 0)

# add preds to predicted data
gam_surge$gam_preds_ada <- exp(preds_capped$ada)
gam_surge$gam_preds_lgbm <- exp(preds_capped$lightgbm)
gam_surge$gam_preds_et <- exp(preds_capped$et)

# calculate ratios of mults
ratios_ada <- gam_surge |> select(id, huc, gam_preds_ada, ros) |>
  group_by(id) |>
  slice(1:2) |>
  pivot_wider(names_from = ros, values_from = gam_preds_ada) |>
  filter(nonros > 1 & ros > 1) |>
  mutate(mult_ratio = ros / nonros,
         mod = "ada")

ratios_lgbm <- gam_surge |> select(id, huc, gam_preds_lgbm, ros) |>
  group_by(id) |>
  slice(1:2) |>
  pivot_wider(names_from = ros, values_from = gam_preds_lgbm) |>
  filter(nonros > 1 & ros > 1) |>
  mutate(mult_ratio = ros / nonros,
         mod = "lgbm")

ratios_et <- gam_surge |> select(id, huc, gam_preds_et, ros) |>
  group_by(id) |>
  slice(1:2) |>
  pivot_wider(names_from = ros, values_from = gam_preds_et) |>
  filter(nonros > 1 & ros > 1) |>
  mutate(mult_ratio = ros / nonros,
         mod = "et")

ratios_comb <- rbind(ratios_ada, ratios_lgbm, ratios_et)

med_vals <- ratios_comb |>
  group_by(mod) |>
  summarize(median = median(mult_ratio))

# visualize ratio of ratios
## PAPER
png("figures/ch3/paper/ml_ratios.png", height = 5, width = 6, units = "in", res = 300)
ratios_comb |>
  ggplot() +
  stat_density(aes(x = mult_ratio, col = mod), geom = "line",
               position = "identity", lwd = 1, bw = 0.1) +
  geom_vline(data = med_vals, aes(xintercept = median, col = mod), lty = "dashed",
             show.legend = FALSE) +
  annotate("text", x = 0.85, y = 3.7,
           label = paste0("median = ", round(median(ratios_ada$mult_ratio), 2)),
           size = 3.5,
           col = "#7570b3") +
  annotate("text", x = 1.5, y = 1.2,
           label = paste0("median = ", round(median(ratios_et$mult_ratio), 2)),
           size = 3.5,
           col = "#e7298a") +
  annotate("text", x = 1.4, y = 1.4,
           label = paste0("median = ", round(median(ratios_lgbm$mult_ratio), 2)),
           size = 3.5,
           col = "#66a61e") +
  scale_x_continuous(limits = c(0.2, 2), breaks = seq(0.2, 2, 0.2)) +
  scale_y_continuous(limits = c(0, 4)) +
  xlab("ROSSR") +
  ylab("Density") +
  scale_color_manual(values = rev(c("#66a61e", "#e7298a", "#7570b3")),
                     labels = rev(c("Light GB", "Extra Trees", "AdaBoost"))) +
  guides(color = guide_legend(override.aes = list(lty = c(1, 1, 1),
                                                  lwd = c(1, 1, 1)))) +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 10),
        legend.position = c(0.86, 0.89),
        legend.background = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key=element_blank())
dev.off()

## PRESENTATION
png("figures/ch3/presentation/ml_ratios.png", height = 5, width = 6, units = "in", res = 300)
ratios_comb |>
  ggplot() +
  stat_density(aes(x = mult_ratio, col = mod), geom = "line",
               position = "identity", lwd = 1, bw = 0.1) +
  geom_vline(data = med_vals, aes(xintercept = median, col = mod), lty = "dashed",
             show.legend = FALSE) +
  annotate("text", x = 0.85, y = 3.7,
           label = paste0("median = ", round(median(ratios_ada$mult_ratio), 2)),
           size = 3.5,
           col = "#7570b3") +
  annotate("text", x = 1.5, y = 1.2,
           label = paste0("median = ", round(median(ratios_et$mult_ratio), 2)),
           size = 3.5,
           col = "#e7298a") +
  annotate("text", x = 1.4, y = 1.4,
           label = paste0("median = ", round(median(ratios_lgbm$mult_ratio), 2)),
           size = 3.5,
           col = "#66a61e") +
  scale_x_continuous(limits = c(0.2, 2), breaks = seq(0.2, 2, 0.2)) +
  scale_y_continuous(limits = c(0, 4)) +
  xlab("ROSSR") +
  ylab("Density") +
  ggtitle("ROSSR Distributions for Top 3 Stressor ML Models") +
  scale_color_manual(values = rev(c("#66a61e", "#e7298a", "#7570b3")),
                     labels = rev(c("Light GB", "Extra Trees", "AdaBoost"))) +
  guides(color = guide_legend(override.aes = list(lty = c(1, 1, 1),
                                                  lwd = c(1, 1, 1)))) +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 10),
        legend.position = c(0.86, 0.89),
        legend.background = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key=element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()



# 10 fold cross-validation for formula without lat/lon ------------
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
results_base2 <- sapply(preds2, function(x) mean((log(peak_data_dt$mult) - x) ^ 2,
                                               na.rm = TRUE))


# 10 fold cross-validation for formula with all available variables (except smp) -------
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
ind <- which(peak_data_dt$swe_av == 0 & is.na(peak_data_dt$snow_dep_av))
peak_data_dt$snow_dep_av[ind] <- 0

data.table::setDT(peak_data_dt)
peak_data_dt <- peak_data_dt[complete.cases(id, dt, mult, huc,
                                            base_med, lat, lon, ros_num,
                                            ros, snow_dep_av, snow_dep_med, snow_dep_min, snow_dep_max, prec_max,
                                            prec_av, prec_min, prec_sum, prec_med, swe_av, swe_med, swe_min, swe_max,
                                            temp_degc_med, temp_degc_av, temp_degc_min, temp_degc_max, melt_av,
                                            melt_med, melt_min, melt_max, elev_av, elev_med, elev_min, elev_max,
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
results_base3 <- sapply(preds3, function(x) mean((log(peak_data_dt$mult) - x) ^ 2,
                                                 na.rm = TRUE))



# 10 fold cross-validation for formula with all available variables (except smp and lat/lon) ----------
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
ind <- which(peak_data_dt$swe_av == 0 & is.na(peak_data_dt$snow_dep_av))
peak_data_dt$snow_dep_av[ind] <- 0

data.table::setDT(peak_data_dt)
peak_data_dt <- peak_data_dt[complete.cases(id, dt, mult, huc,
                                            base_med, ros_num,
                                            ros, snow_dep_av, snow_dep_med, snow_dep_min, snow_dep_max, prec_max,
                                            prec_av, prec_min, prec_sum, prec_med, swe_av, swe_med, swe_min, swe_max,
                                            temp_degc_med, temp_degc_av, temp_degc_min, temp_degc_max, melt_av,
                                            melt_med, melt_min, melt_max, elev_av, elev_med, elev_min, elev_max,
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
results_base4 <- sapply(preds4, function(x) mean((log(peak_data_dt$mult) - x) ^ 2,
                                                 na.rm = TRUE))



