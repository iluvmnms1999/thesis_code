library(ggmap)
library(ggplot2)
library(usmap)
library(sf)
library(tidyverse)
library(data.table)

# read in huc regions
huc8_west <- readRDS("data-raw/huc8_west.rds")

# read in remaining streamgage locations
peaks_ref <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
peaks_ref <- st_as_sf(peaks_ref)

# read in remaining snotel station locations
snotel_id <- read.csv("data-raw/snotel_id.csv") |>
  mutate(huc8 = as.numeric(substr(str_extract_all(huc, "\\d+\\)$"), 1, 8)))

match <- which(snotel_id$huc8 %in% unique(peaks_ref$huc))
snotel_keep <- snotel_id[match, ]
snotel_sf <- sf::st_as_sf(snotel_keep, coords = c("lon", "lat"),
                          crs = sf::st_crs(peaks_ref)) |>
  dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                lat = sf::st_coordinates(geometry)[,2],
                id = str_replace(str_extract_all(site_name, "\\d+\\)$"), "\\)",
                                 ""))
# adjust text positions for map labels
snotel_sf$nudge_y <- .035
snotel_sf$nudge_x <- .03
snotel_sf$nudge_x[snotel_sf$id == "540"] <- .065
snotel_sf$nudge_y[snotel_sf$id == "540"] <- 0
snotel_sf$nudge_x[snotel_sf$id == "539"] <- .065
snotel_sf$nudge_y[snotel_sf$id == "539"] <- 0
snotel_sf$nudge_x[snotel_sf$id == "541"] <- .04
snotel_sf$nudge_y[snotel_sf$id == "541"] <- -.03
snotel_sf$nudge_x[snotel_sf$id == "784"] <- .05
snotel_sf$nudge_x[snotel_sf$id == "1242"] <- .05



png("figures/ch2/output_fig/fig2_11.png", height = 5, width = 9, units = "in",
    res = 600)
# save your own Google API key to the data-raw folder and upload
google_API_key <- scan(paste0(getwd(), "/data-raw/google_api.txt"), what = "")
register_google(key = google_API_key)

# set bbox
bbox <- setNames(st_bbox(huc8_west$geometry[huc8_west$huc8 == 16050102]),
                 c("left", "bottom", "right", "top"))
basemap_streets <- get_map(maptype = "roadmap", location = bbox, zoom = 9)
street_map <- ggmap(basemap_streets)

# crop to huc region
nev_shp <- huc8_west$geometry[huc8_west$huc8 == 16050102]

street_map +
  geom_sf(data = nev_shp,
          inherit.aes = FALSE,
          color = "black", lwd = 0.75, fill = NA) +
  geom_sf(data = filter(peaks_ref, id == 10349300), aes(color = "#e08214"),
          size = 2) +
  geom_sf(data = filter(snotel_sf, huc8 == 16050102), aes(color = "#542788"),
          size = 2) +
  scale_color_manual(values = c("#542788", "#e08214"),
                     labels = c("SNOTEL", "Streamgage"), name = "") +
  geom_text(data = filter(snotel_sf, huc8 == 16050102), aes(lon, lat, label = id),
            size = 3, nudge_y = filter(snotel_sf, huc8 == 16050102)$nudge_y,
            nudge_x = filter(snotel_sf, huc8 == 16050102)$nudge_x) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.83, 0.96),
        legend.background = element_blank(),
        legend.text = element_text(size = 11),
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.2, "cm"),
        legend.key = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())
dev.off()
