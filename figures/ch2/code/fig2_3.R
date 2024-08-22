# libraries
library(ggplot2)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(gtable)
library(grid)
library(data.table)

# get all periods of record for all stations
# the following for loop takes ~10 minutes to run
pors_all <- data.frame()
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "WY", "WA", "AZ", "UT")
for (x in seq_along(states)) {
  # read in streamflow data
  rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[x], ".RDS"))
  rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[x], ".RDS"))

  # combine data and convert to data table
  rhv_all <- rbind(rhv_tot, rhv_miss)
  data.table::setDT(rhv_all)

  # calculate por
  pors <- rhv_all[, .(por = difftime(datetime[.N], datetime[1],
                                     units = "days") |>
                        as.numeric() / 365,
                      year_start = year(datetime[1]),
                      year_end = year(datetime[.N])),
                  by = id]

  # combine with state abb
  comb <- cbind(rep(states[x], nrow(pors)), pors)

  # add to por overall data frame
  pors_all <- rbind(pors_all, comb)
}

# only keep gages with peaks
peaks <- readRDS("data-raw/peaks_fin/peaks_base_med_ref.RDS")
pors_peaks <- pors_all[id %in% unique(peaks$id)]

png("figures/ch2/output_fig/fig2_3.png", width = 6, height = 3, units = "in",
    res = 600)
# histogram of por length
g1 <- pors_peaks |>
  ggplot(aes(x = por)) +
  geom_histogram(breaks = seq(0, 50, 5), col = "gray", fill = "#023E8A",
                 closed = "left") +
  xlab("Length of Period of Record (in years)") +
  ylab("Count") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
  scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 100)) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank())
# histogram of start year
g2 <- pors_peaks |>
  ggplot(aes(x = year_start)) +
  geom_histogram(breaks = seq(1980, 2025, 5), col = "gray", fill = "#023E8A",
                 closed = "left") +
  xlab("Period of Record Start Year") +
  ylab("Count") +
  scale_x_continuous(limits = c(1980, 2025), breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1250, 250)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.grid.minor = element_blank())
# plot plot windows the same size
g1 <- ggplotGrob(g1)
g2 <- ggplotGrob(g2)
g <- cbind(g1, g2, size = "first")
g$heights <- unit.pmax(g1$heights, g2$heights)
grid.newpage()
grid.draw(g)
dev.off()
