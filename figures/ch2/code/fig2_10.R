library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)

peaks <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")

# look at distribution of empirical surge ratio
emp_surge <- peaks |>
  group_by(id, ros) |>
  summarize(med_surge = median(peakflow),
            count = n()) |>
  pivot_wider(names_from = "ros", values_from = c("med_surge", "count")) |>
  filter(`count_non-ros` >= 2, count_ros >= 2) |>
  mutate(emp_rat = med_surge_ros / `med_surge_non-ros`)

# density plot
png("figures/ch2/output_fig/fig2_10.png", height = 5, width = 6, units = "in",
    res = 600)
emp_surge |>
  ggplot(aes(x = emp_rat)) +
  geom_density(bw = 0.09) + # smooth it a bit
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5)) +
  geom_vline(aes(xintercept = median(emp_rat)), col = "gray45",
             lty = "dashed") +
  annotate("text", x = 0.78, y = 0, label = "median = 1.01", color = "gray45",
           size = 3.5) +
  geom_vline(aes(xintercept = mean(emp_rat)), col = "gray45") +
  annotate("text", x = 1.27, y = 0, label = "mean = 1.05", color = "gray45",
           size = 3.5) +
  xlab("Ratio") +
  ylab("Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11))
dev.off()
