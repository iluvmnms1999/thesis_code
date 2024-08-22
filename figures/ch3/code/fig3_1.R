library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

# define model and train model on profiles
gam_data <- readRDS("data-raw/modeling/gam_datav3.rds")

# replace NA snow depth with 0 when swe is 0
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
ind <- which(peak_data_dt$swe_av == 0 & is.na(peak_data_dt$snow_dep_av))
peak_data_dt$snow_dep_av[ind] <- 0

# reformat peak data so we have the same variables
data.table::setDT(peak_data_dt)
peak_data <- peak_data_dt[, .(id, dt, mult, huc, med_bf = base_med, lat, lon,
                              ros,
                              snowdep = snow_dep_av,
                              prec_med = prec_max,
                              swe = swe_av, temp = temp_degc_med)]

# fit gam with snow depth
gam_obj <- mgcv::gam(log(mult) ~
                         s(temp) +
                         s(snowdep) +
                         s(prec_med) +
                         s(swe) +
                         s(log(med_bf)) +
                         s(lat, lon, bs = "sos", k = 25),
                     data = peak_data)

# make preds
preds <- predict(gam_obj, gam_data)
preds_capped <- pmax(preds, 0)

# add exponentiated preds to predicted data
gam_data$gam_preds <- exp(preds_capped)

# calculate ratios of mults
ratios <- gam_data |>
  select(id, huc, gam_preds, ros) |>
  group_by(id) |>
  slice(1:2) |>
  pivot_wider(names_from = ros, values_from = gam_preds) |>
  filter(nonros > 1 & ros > 1) |>
  mutate(mult_ratio = ros / nonros,
         form = "wsd")

# fit gam without snow depth
gam_obj <- mgcv::gam(log(mult) ~
                       s(temp) +
                       s(prec_med) +
                       s(swe) +
                       s(log(med_bf)) +
                       s(lat, lon, bs = "sos", k = 25),
                     data = peak_data)

# make preds
preds <- predict(gam_obj, gam_data)
preds_capped <- pmax(preds, 0)

# add exponentiated preds to predicted data
gam_data$gam_preds <- exp(preds_capped)

# run this after running GAM without snow depth
ratios_wosd <- gam_data |>
  select(id, huc, gam_preds, ros) |>
  group_by(id) |>
  slice(1:2) |>
  pivot_wider(names_from = ros, values_from = gam_preds) |>
  filter(nonros > 1 & ros > 1) |>
  mutate(mult_ratio = ros / nonros,
         form = "wosd")

ratios_comb <- rbind(ratios, ratios_wosd)

med_vals <- ratios_comb |>
  group_by(form) |>
  summarize(median = median(mult_ratio))

# visualize ratio of ratios
png("figures/ch3/output_fig/fig3_1.png", height = 5, width = 6, units = "in",
    res = 600)
ratios_comb |>
  ggplot() +
  stat_density(aes(x = mult_ratio, col = form), geom = "line",
               position = "identity", lwd = 1, bw = 0.1) +
  geom_vline(data = med_vals, aes(xintercept = median, col = form),
             lty = "dashed",
             show.legend = FALSE) +
  annotate("text", x = 1.34, y = 1.8, col = "blue",
           label = paste0("median = ", round(median(ratios$mult_ratio),
                                             2)),
           size = 3.5) +
  annotate("text", x = 0.85, y = 3.5, col = "orange",
           label = paste0("median = ", round(median(ratios_wosd$mult_ratio),
                                             2)),
           size = 3.5) +
  scale_x_continuous(limits = c(0.4, 2), breaks = seq(0.4, 2, 0.2)) +
  scale_y_continuous(limits = c(0, 4)) +
  xlab("ROSSR") +
  ylab("Density") +
  scale_color_manual(values = rev(c("blue", "orange")),
                     labels = rev(c("With Snow Depth", "Without Snow Depth"))) +
  guides(color = guide_legend(override.aes = list(lty = c(1, 1),
                                                  lwd = c(1, 1)))) +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 10),
        legend.position = c(0.825, 0.905),
        legend.background = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key = element_blank())

dev.off()
