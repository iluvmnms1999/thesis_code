library(stressor)
library(reticulate)
library(data.table)
library(tidyverse)

# set up environment to use stressor
# use your own path to python
use_python("/Users/emmawatts/anaconda3/bin/python")
stressor::create_virtualenv()
reticulate::use_virtualenv("stressor-env2024-06-24075127184412")

peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
ind <- which(peak_data_dt$swe_av == 0 & is.na(peak_data_dt$snow_dep_av))
peak_data_dt$snow_dep_av[ind] <- 0

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
ratios_ada <- gam_surge |>
  select(id, huc, gam_preds_ada, ros) |>
  group_by(id) |>
  slice(1:2) |>
  pivot_wider(names_from = ros, values_from = gam_preds_ada) |>
  filter(nonros > 1 & ros > 1) |>
  mutate(mult_ratio = ros / nonros,
         mod = "ada")

ratios_lgbm <- gam_surge |>
  select(id, huc, gam_preds_lgbm, ros) |>
  group_by(id) |>
  slice(1:2) |>
  pivot_wider(names_from = ros, values_from = gam_preds_lgbm) |>
  filter(nonros > 1 & ros > 1) |>
  mutate(mult_ratio = ros / nonros,
         mod = "lgbm")

ratios_et <- gam_surge |>
  select(id, huc, gam_preds_et, ros) |>
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
png("figures/ch3/output_fig/fig3_10.png", height = 5, width = 6, units = "in",
    res = 600)
ratios_comb |>
  ggplot() +
  stat_density(aes(x = mult_ratio, col = mod), geom = "line",
               position = "identity", lwd = 1, bw = 0.1) +
  geom_vline(data = med_vals, aes(xintercept = median, col = mod),
             lty = "dashed",
             show.legend = FALSE) +
  annotate("text", x = 0.85, y = 3.7,
           label = paste0("median = ",
                          round(median(ratios_ada$mult_ratio), 2)),
           size = 3.5,
           col = "#7570b3") +
  annotate("text", x = 1.5, y = 1.2,
           label = paste0("median = ",
                          round(median(ratios_et$mult_ratio), 2)),
           size = 3.5,
           col = "#e7298a") +
  annotate("text", x = 1.4, y = 1.4,
           label = paste0("median = ",
                          round(median(ratios_lgbm$mult_ratio), 2)),
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
        legend.key = element_blank())
dev.off()
