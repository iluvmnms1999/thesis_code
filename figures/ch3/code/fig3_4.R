library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(gridExtra)
library(ggh4x)
# next four packages all for getting the raster data correct
library(terra)
library(stars)
library(sf)
library(gstat)

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
                       s(snowdep) +
                       s(prec) +
                       s(swe) +
                       s(log(med_bf)) +
                       s(lat, lon, bs = "sos", k = 25),
                     data = peak_data)

# plot silenty returns the data - select = -1 makes it so default plots don't
#   show
tdata <- plot(gam_obj, select = -1)

# make lat/lon plot -------------------------------------------------------
t_6 <- tdata[[6]]

# create data frame with coordinates and values
tdf <- data.frame(lon = t_6$lo, lat = t_6$la,
                  value = t_6$fit[, 1]) |>
  tidyr::drop_na()

# convert to a spatial data frame
tdf_sf <- sf::st_as_sf(tdf, coords = c("lon", "lat"), crs = 4326)

# read in a template raster (obtained from 4km prism elevation)
# - https://www.prism.oregonstate.edu/normals/
prism_template <- terra::rast(paste0("data-raw/prism/PRISM_us_dem_4km_asc/",
                                     "PRISM_us_dem_4km_asc.asc"))

# create map of states and project them to the same extent as prism
states <- maps::map("state", fill = TRUE, plot = FALSE)
states <- st_as_sf(states) |>
  dplyr::filter(ID %in% c("washington", "oregon", "california",
                          "nevada", "idaho", "utah", "arizona",
                          "montana", "wyoming", "colorado",
                          "new mexico"))
states <- st_transform(states, crs = crs(prism_template))

# transform spatial points as well
tdf_sf <- st_transform(tdf_sf, crs = crs(prism_template))

# crop prism data according to a bounding box of states; because
# state geometry is whack, we create a geometry out of the bounding box only
bounding_box <- st_bbox(states) %>% st_as_sfc()
prism_crop <- crop(prism_template, bounding_box)
tdf_sf_2 <- st_intersection(tdf_sf, bounding_box)

# now aggregate the PRISM data to be similar in resolution to points
prism_crop_2 <- aggregate(prism_crop, 30)

# now use inverse distance weighting to interpolate between the points
new_rast <- gstat::idw(value ~ 1, locations = tdf_sf_2,
                       newdata = st_as_stars(prism_crop_2))

# plot rasterized points on a plot with the states
png("figures/ch3/output_fig/fig3_4.png", width = 8, height = 5, units = "in",
    res = 600)
ggplot() +
  geom_stars(data = new_rast) +
  geom_sf(data = states, fill = NA) +
  xlab("") +
  ylab("") +
  guides(fill = guide_legend(title = "Effect Size", reverse = TRUE)) +
  scale_fill_gradient(breaks = c(0, .5, 1, 1.5, 2, 2.5),
                      low = "#d0d1e6",
                      high = "#016450",
                      na.value = "transparent",
                      labels = rev(c("(2.5, 3]", "(2, 2.5]",
                                     "(1.5, 2]", "(1, 1.5]",
                                     "(0.5, 1]", "(0, 0.5]"))) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
dev.off()
