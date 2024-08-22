# libraries
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

# get all initial streamgages
usgs_huc <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")
peaks <- readRDS("data-raw/peaks_fin/peaks_tot.RDS")
peaks <- peaks[, huc := rep(0, nrow(peaks))]
for (i in seq_along(usgs_huc$site_no)) {
  # add huc variable to peak data
  peaks[peaks$id ==
          formatC(usgs_huc$site_no[i],
                  width = 8,
                  format = "d",
                  flag = "0")]$huc <-
    rep(usgs_huc$huc8[i],
        times = nrow(peaks[peaks$id == formatC(usgs_huc$site_no[i], width = 8,
                                               format = "d", flag = "0")]))
}
peaks$id <- as.integer(peaks$id)
usgs_id <- read.csv("data-raw/usgs_id.csv")
names(usgs_id)[1] <- "id"
peaks_all <- peaks |>
  left_join(usgs_id[, c(1, 4:5)], join_by(id))
peaks_all <- sf::st_as_sf(peaks_all, coords = c("dec_long_va", "dec_lat_va"),
                          crs = sf::st_crs(peaks_ref)) |>
  dplyr::mutate(lon = sf::st_coordinates(geometry)[, 1],
                lat = sf::st_coordinates(geometry)[, 2])


# get all snotel stations kept after association by huc
snotel_id <- read.csv("data-raw/snotel_id.csv") |>
  mutate(huc8 = as.numeric(substr(str_extract_all(huc, "\\d+\\)$"), 1, 8)))

match <- which(snotel_id$huc8 %in% unique(peaks_ref$huc))
snotel_keep <- snotel_id[match, ]
snotel_sf <- sf::st_as_sf(snotel_keep, coords = c("lon", "lat"),
                          crs = sf::st_crs(peaks_ref)) |>
  dplyr::mutate(lon = sf::st_coordinates(geometry)[, 1],
                lat = sf::st_coordinates(geometry)[, 2],
                id = str_replace(str_extract_all(site_name, "\\d+\\)$"),
                                 "\\)", ""))
# get all initial snotel stations
snotel_filt <- snotel_id |>
  filter(!(state %in% c("AK", "SD")))
snotel_all <- sf::st_as_sf(snotel_filt, coords = c("lon", "lat"),
                           crs = sf::st_crs(peaks_ref)) |>
  dplyr::mutate(lon = sf::st_coordinates(geometry)[, 1],
                lat = sf::st_coordinates(geometry)[, 2])


png("figures/ch2/output_fig/fig2_4.png", height = 5, width = 9, units = "in",
    res = 600)
# plot western states with huc8 regions and all usgs/snotel stations
g1 <- ggplot() +
  geom_sf(data = west, col = "black", fill = "gray95", lwd = .5) +
  geom_sf(data = huc8_west, col = "gray50", fill = NA) +
  geom_sf(data = peaks_all, color = "#e08214", size = 1.5, alpha = 0.25,
          shape = 1) +
  geom_sf(data = snotel_all, color = "#2d004b", size = 1.5, alpha = 0.5,
          shape = 3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 10))

# plot western states with huc8 regions and usgs/snotel stations retained
#   because they're in the same hucs
g2 <- ggplot() +
  geom_sf(data = west, col = "black", fill = "gray95", lwd = .5) +
  geom_sf(data = huc8_west, col = "gray50", fill = NA) +
  geom_sf(data = peaks_ref, aes(color = "#e08214"), size = 1.5, alpha = 0.25,
          shape = 1) +
  geom_sf(data = snotel_sf, aes(color = "#2d004b"), size = 1.5, alpha = 0.5,
          shape = 3) +
  scale_color_manual(values = c("#2d004b", "#e08214"),
                     labels = c("SNOTEL", "Streamgage"), name = "") +
  guides(color = guide_legend(override.aes = list(shape = c(3, 1),
                                                  alpha = 1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.82, 0.96),
        legend.background = element_blank(),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.2, "cm"),
        axis.text = element_text(size = 10))

grid.arrange(g1, g2, nrow = 1)
dev.off()
