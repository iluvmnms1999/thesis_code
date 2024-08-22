# libraries
library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)

# load peak data
peaks <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")

# in which month do peaks usually occur?
month_counts <- peaks |>
  mutate(month = month(dt)) |>
  group_by(ros, month) |>
  summarise(count = n()) |>
  group_by(ros) |>
  mutate(count_prop = count / sum(count))


png("figures/ch2/output_fig/fig2_7.png", width = 5, height = 4, units = "in",
    res = 600)
ggplot(month_counts, aes(x = as.factor(month), y = count_prop, group = ros,
                         color = ros)) +
  geom_path(lwd = 0.9) +
  geom_point() +
  xlab("Month") +
  ylab("Proportion of Peaks") +
  scale_y_continuous(limits = c(0, 0.4)) +
  scale_color_manual(values = c("#bf812d", "#41ab5d"),
                     labels = c("Non-ROS", "ROS")) +
  theme_bw() +
  theme(legend.position = c(0.83, 0.88),
        legend.background = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key = element_blank())
dev.off()
