library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)

peaks <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")

# look at distribution of empirical surge ratio
emp_surge <- peaks |>
  group_by(id, ros) |>
  summarize(med_surge = median(mult),
            count = n()) |>
  pivot_wider(names_from = "ros", values_from = c("med_surge", "count")) |>
  filter(`count_non-ros` >= 2, count_ros >= 2) |>
  mutate(emp_rat = med_surge_ros / `med_surge_non-ros`)


png("figures/ch2/output_fig/fig2_9.png", height = 2, width = 5, units = "in",
    res = 600)
emp_surge |>
  ggplot() +
  geom_boxplot(aes(x = emp_rat), outlier.color = "red", outlier.shape = 18) +
  scale_x_continuous(trans = scales::log2_trans(),
                     breaks = scales::trans_breaks("log2", function(x) 2 ^ x,
                                                   n = 8),
                     limits = c(0.01, 2 ^ 6),
                     labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_y_continuous(limits = c(-.4, .4)) +
  xlab("Ratio (log2-scale)") +
  ylab("") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
dev.off()
