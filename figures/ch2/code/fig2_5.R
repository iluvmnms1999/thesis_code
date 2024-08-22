# libraries
library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)

peaks <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")

# look at distributions of important variables (temp, swe, precip, and surge)
png("figures/ch2/output_fig/fig2_5.png", width = 7, height = 6, units = "in",
    res = 600)
g1 <- peaks |>
  ggplot() +
  stat_density(aes(x = temp_degc_av, color = ros),
               geom = "line", position = "identity", lwd = 1) +
  stat_density(aes(x = temp_degc_av, color = "gray60"), linetype = "dashed",
               geom = "line", position = "identity", inherit.aes = FALSE) +
  scale_color_manual(values = c("gray60", "#bf812d", "#41ab5d"),
                     labels = c("Overall", "Non-ROS", "ROS")) +
  guides(color = guide_legend(override.aes = list(lty = c("dashed", "solid",
                                                          "solid"),
                                                  lwd = c(0.5, 1, 1)))) +
  xlab("Temperature (in \u00B0C)") +
  ylab("") +
  scale_x_continuous(limits = c(-10, 20), breaks = seq(-10, 20, 5)) +
  scale_y_continuous(limits = c(0, 0.2)) +
  theme_bw() +
  theme(legend.position = c(0.835, 0.85),
        legend.background = element_blank(),
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.5, "cm"),
        panel.grid.minor = element_blank())

g2 <- peaks |>
  ggplot() +
  stat_density(aes(x = prec_max, color = ros),
               geom = "line", position = "identity", lwd = 1,
               show.legend = FALSE) +
  stat_density(aes(x = prec_max, color = "gray60"), linetype = "dashed",
               geom = "line",
               position = "identity", inherit.aes = FALSE,
               show.legend = FALSE) +
  scale_color_manual(values = c("gray60", "#bf812d", "#41ab5d")) +
  xlab("Precipitation (in mm, log2-scale)") +
  ylab("Density") +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x,
                                                   n = 8),
                     limits = c(1, 2 ^ 9)) +
  scale_y_continuous(breaks = seq(0, 0.4, 0.1), limits = c(0, 0.4)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

g3 <- peaks |>
  ggplot() +
  stat_density(aes(x = swe_av, color = ros),
               geom = "line", position = "identity", lwd = 1,
               show.legend = FALSE) +
  stat_density(aes(x = swe_av, color = "gray60"), linetype = "dashed",
               geom = "line",
               position = "identity", inherit.aes = FALSE,
               show.legend = FALSE) +
  scale_color_manual(values = c("gray60", "#bf812d", "#41ab5d")) +
  xlab("SWE (in mm, log2-scale)") +
  ylab("") +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x,
                                                   n = 6),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.1), limits = c(0, 0.3)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

g4 <- peaks %>%
  ggplot() +
  stat_density(aes(x = mult, color = ros),
               geom = "line", position = "identity", lwd = 1,
               show.legend = FALSE) +
  stat_density(aes(x = mult, color = "gray60"), linetype = "dashed",
               geom = "line",
               position = "identity", inherit.aes = FALSE,
               show.legend = FALSE) +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x,
                                                   n = 6),
                     limits = c(1, 2 ^ 12)) +
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5)) +
  xlab("Surge (cfs, log2-scale)") +
  ylab("Density") +
  scale_color_manual(values = c("gray60", "#bf812d", "#41ab5d")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

grid.arrange(g4, g1, g2, g3, nrow = 2)
dev.off()
