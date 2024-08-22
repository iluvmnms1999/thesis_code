library(ggplot2)
library(usmap)
library(sf)
library(tidyverse)
library(data.table)
library(gridExtra)

# get area of interest
states <- us_map(regions = "state")
west <- subset(states, abbr %in% c("NV", "CA", "CO", "ID", "MT", "NM",
                                   "OR", "UT", "WA", "AZ", "WY"))

# get huc 8 regions in area of interest
huc8_west <- readRDS("data-raw/huc8_west.rds")

# get all streamgages kept after association by huc
peaks_ref <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
peaks_ref <- st_as_sf(peaks_ref)
setDT(peaks_ref)

## get periods of record
# which stations do we need period of record for?
stations <- peaks_ref |>
  select(state, id) |>
  group_by(id) |>
  slice(1) |>
  as.data.frame() |>
  setDT()

stations <- stations[, por := 0]

# get lengths of period of records for each unique station
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
for (x in seq_along(states)) {
  # get all streamflow data
  rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[x], ".RDS"))
  rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[x], ".RDS"))
  rhv_all <- rbind(rhv_tot, rhv_miss)
  data.table::setDT(rhv_all)
  rhv_all$id <- as.integer(rhv_all$id)

  # subset state-specific id's needed
  stat_temp <- stations[state == states[x]]

  por <- c()
  for (i in seq_len(nrow(stat_temp))) {
    temp <- rhv_all[id == stat_temp$id[i]]
    por[i] <- difftime(temp$datetime[nrow(temp)], temp$datetime[1],
                       unit = "days") / 365
  }
  stations[state == states[x]]$por <- por
}

# how many surges at each station?
surges_count <- peaks_ref[, .(surge_count = .N,
                              huc = max(huc)), by = id]

ros_count <- peaks_ref[ros == "ros", .(ros_count = .N), by = id]

stat_surge <- left_join(stations, surges_count, by = join_by(id))
stat_tot <- left_join(stat_surge, ros_count, by = join_by(id))
stat_tot[is.na(stat_tot)] <- 0
names(stat_tot)[5] <- "huc8"

stat_huc <- left_join(stat_tot, huc8_west, by = join_by(huc8))
stat_huc <- stat_huc[, -7]
stat_huc <- st_as_sf(stat_huc)

# summarize by huc
huc_sums <- stat_huc |>
  reframe(surge_prop = surge_count / por,
          ros_prop_tot = ros_count / surge_count,
          ros_prop_year = ros_count / por,
          huc8 = huc8,
          geometry = geometry) |>
  group_by(huc8) |>
  summarize(surge = ifelse(n() == 1, surge_prop, median(surge_prop)),
            ros_tot = mean(ros_prop_tot),
            ros_year = mean(ros_prop_year)) |>
  left_join(huc8_west, by = join_by(huc8)) |>
  select(-5) |>
  setDT() |>
  st_as_sf()


png("figures/ch2/output_fig/fig2_6.png", height = 7.5, width = 11, units = "in",
    res = 600)
g1 <- ggplot() +
  geom_sf(data = west, col = "black", fill = "gray95", lwd = .5) +
  geom_sf(data = huc8_west, col = "gray50", fill = NA) +
  geom_sf(data = huc_sums, aes(fill = log2(surge)), inherit.aes = FALSE) +
  theme_bw() +
  guides(fill = guide_legend(title = "Surge\nCount", reverse = TRUE,
                             label.position = "bottom",
                             direction = "horizontal",
                             nrow = 2, byrow = TRUE)) +
  scale_fill_gradient(breaks = waiver(), n.breaks = 6, high = "#deebf7",
                      low = "#08306b",
                      labels = c("(4, 4.29]", "(2, 4]", "(1, 2]", "(0.5, 1]",
                                 "(0.25, 0.5]", "(0.13, 0.25]",
                                 "(0.06, 0.13]", "[0.03, 0.06]")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.margin = margin(5.5, 6, 5.5, 5.5),
        legend.title = element_text(size = 15, vjust = 0.75),
        legend.key.size = unit(.25, "in"),
        legend.key.width = unit(.75, "in"),
        legend.key.height = unit(.25, "in"),
        axis.text = element_text(size = 12))

## choropleth map of prop of ros surges in each huc
g2 <- ggplot() +
  geom_sf(data = west, col = "black", fill = "gray95", lwd = .5) +
  geom_sf(data = huc8_west, col = "gray50", fill = NA) +
  geom_sf(data = huc_sums, aes(fill = ros_tot), inherit.aes = FALSE) +
  theme_bw() +
  guides(fill = guide_legend(title = "ROS\nProp.", reverse = FALSE,
                             label.position = "bottom")) +
  scale_fill_gradient(breaks = c(0, .25, .5, 1), low = "#e5f5e0",
                      high = "#00441b",
                      labels = rev(c("(0.75, 1]", "(0.50, 0.75]",
                                     "(0.25, 0.50]", "[0, 0.25]"))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.margin = margin(5.5, 6, 5.5, 5.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15, vjust = 1.5),
        legend.key.size = unit(.25, "in"),
        legend.key.width = unit(.75, "in"),
        legend.key.height = unit(.25, "in"),
        axis.text = element_text(size = 12))

cowplot::plot_grid(g1, g2, align = "h")
dev.off()
