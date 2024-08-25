# ~ 10 min to run
# This script calculates the baseflow prior to each peak using the median
#   streamflow over the two weeks prior to peak occurrence. If two peaks occur
#   within a time period of less than two weeks, the median over that smaller
#   time period is taken.

library(data.table)
library(tidyverse)
peaks <- readRDS("data-raw/peaks/peaks_tot.RDS")
data.table::setDT(peaks)

# add base_med column to peaks df beforehand
peaks <- peaks[, base_med := 0]

# calculate baseflow
states <- c("NV", "CA", "CO", "ID", "MT", "NM", "OR", "UT", "WA", "AZ", "WY")
for (x in seq_along(states)) {
  rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_",
                            states[x], ".RDS")) |>
    setDT()
  rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_",
                             states[x], ".RDS")) |>
    setDT()
  rhv_all <- rbindlist(list(rhv_tot, rhv_miss))

  peaks_sub <- peaks[state == states[x]]
  # using data.table for this step automatically converts to days but some are
  # wrong -- reported as days when they are actually hours
  peaks_diff <- peaks_sub |>
    group_by(id) |>
    mutate(time_diff = c(as.difftime(0, units = "secs"), diff(dt)))
  data.table::setDT(peaks_diff)
  vec <- c()
  for (i in seq_len(nrow(peaks_diff))) {

    # account for peaks that have less than two weeks time between them
    if (peaks_diff$time_diff[i] < 1209600 && peaks_diff$time_diff[i] > 0) {
      temp <- rhv_all[datetime %in% seq(peaks_diff$dt[i - 1],
                                        peaks_diff$dt[i], by = "hour")
                      & id == peaks_diff$id[i]]
      vec[i] <- median(temp$max_flow)

    # take median over two weeks prior
    } else {
      temp <- rhv_all[datetime %in% seq(peaks_diff$dt[i] - 1209600,
                                        peaks_diff$dt[i], by = "hour")
                      & id == peaks_diff$id[i]]
      vec[i] <- median(temp$max_flow)
    }
  }
  peaks[state == states[x]]$base_med <- vec
}
r

saveRDS(peaks, "data-raw/peaks/peaks_base_med_ref.RDS")
