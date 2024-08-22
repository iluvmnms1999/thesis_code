library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(gridExtra)
library(ggh4x)

# load data matrix
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
gam_data <- readRDS("data-raw/modeling/gam_datav3.rds")

# reformat peak data so we have the same variables
data.table::setDT(peak_data_dt)
peak_data <- peak_data_dt[, .(id, dt, mult, huc, med_bf = base_med, lat, lon,
                              ros, snowdep = snow_dep_av, prec = prec_max,
                              swe = swe_av, temp = temp_degc_med)]

# fit gam
gam_obj <- mgcv::gam(log(mult) ~
                       s(temp) +
                       s(prec) +
                       s(swe) +
                       s(log(med_bf)) +
                       s(lat, lon, bs = "sos", k = 25),
                     data = peak_data)

# plot silenty returns the data - select = -1 makes it so default plots don't
#   show
tdata <- plot(gam_obj, select = -1)

data_list <- vector("list", 4)
var <- c("Temperature", "Precipitation", "SWE", "Baseflow")

for (i in seq_len(length(data_list))) {
  temp <- tdata[[i]]
  data_list[[i]] <- data.frame(x = temp$x, y = temp$fit[, 1], se = temp$se,
                               var = var[i])
}

data_final <- data.table::rbindlist(data_list)

x_scales <- list(
  scale_x_continuous(limits = c(-10, 15), breaks = seq(-10, 15, 5)),
  scale_x_continuous(limits = c(0, 300), breaks = seq(0, 300, 100)),
  scale_x_continuous(limits = c(0, 2500), breaks = seq(0, 2500, 500)),
  scale_x_continuous(limits = c(-30, 40), breaks = seq(-30, 40, 10))
)

y_scales <- list(
  scale_y_continuous(limits = c(-5, 15), breaks = seq(-5, 15, 5)),
  scale_y_continuous(limits = c(-1, 4), breaks = seq(-1, 4, 1)),
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 1)),
  scale_y_continuous(limits = c(-2, 3), breaks = seq(-2, 3, 1))
)


png("figures/ch3/output_fig/fig3_6.png", width = 9, height = 5, units = "in",
    res = 600)
ggplot(data_final, aes(x = x, y = y)) +
  geom_line() +
  geom_rug(position = position_jitter(5), sides = "b") +
  geom_line(aes(x = x, y = y + se), inherit.aes = FALSE, lty = "dashed") +
  geom_line(aes(x = x, y = y - se), inherit.aes = FALSE, lty = "dashed") +
  geom_hline(aes(yintercept = 0), lty = "longdash", color = "gray45",
             lwd = 0.3) +
  scale_y_continuous(limits = c(-5, 15), breaks = seq(-5, 15, 5)) +
  xlab("Value") +
  ylab("Effect Size") +
  facet_nested_wrap(~var, scales = "free") +
  facetted_pos_scales(x = x_scales, y = y_scales) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        strip.text.x = element_text(size = 12))
dev.off()
