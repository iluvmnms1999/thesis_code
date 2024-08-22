# libraries
library(ggplot2)
library(usmap)
library(sf)
library(tidyverse)

# get state regions
states <- us_map(regions = "state")
west <- subset(states, abbr %in% c("NV", "CA", "CO", "ID", "MT", "NM",
                                   "OR", "UT", "WA", "AZ", "WY"))

# get state centroids for labeling
centroid_labels <- usmapdata::centroid_labels("states") |>
  mutate(lon = st_coordinates(geom)[, 1],
         lat = st_coordinates(geom)[, 2]) |>
  filter(abbr %in% c("NV", "CA", "CO", "ID", "MT", "NM",
                     "OR", "UT", "WA", "AZ", "WY"))

# plot states in region of interest
png("figures/ch2/output_fig/fig2_1.png", height = 5, width = 5, units = "in",
    res = 600)
ggplot() +
  geom_sf(data = west, col = "black", fill = "gray95", lwd = .5) +
  geom_text(data = centroid_labels, aes(x = lon, y = lat, label = abbr),
            size = 3) +
  xlab("") +
  ylab("") +
  theme_bw()
dev.off()
