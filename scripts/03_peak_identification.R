# ~ 30 min to run
# This script performs peak detection on all streamflow data and combines
#   peaks into one large data frame.

source("R/peakdf.R")

# get peaks for all states
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "WY", "WA", "AZ", "UT")
usgs_fs_cl <- readRDS("data-raw/usgs_fs/usgs_fs_alm.RDS")
data.table::setDT(usgs_fs_cl)

for (x in seq_along(states)) {
  usgs_fs <- usgs_fs_cl[state == states[x] & !is.na(discharge) & minpeak < 1]
  data.table::setDT(usgs_fs)
  peaks_list <- vector("list", length = nrow(usgs_fs))

  # begin loop
  rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[x], ".RDS"))
  data.table::setDT(rhv_tot)
  vec <- c()
  for (i in seq_len(nrow(usgs_fs))) {
    temp <- rhv_tot[id == formatC(usgs_fs$site_no[i],
                                  width = 8,
                                  format = "d",
                                  flag = "0")]
    if (nrow(temp) == 0) {
      peaks_list[[i]] <- NA
    } else {
      peaks_list[[i]] <- peakdf(df = temp, minpeak = usgs_fs$minpeak[i])
    }

    if (!inherits(peaks_list[[i]], "data.frame")) {
      vec[i] <- i
    }
  }

  if (!inherits(vec, "NULL")) {
    peaks_list2 <- peaks_list[-vec[which(!is.na(vec))]]
  } else {
    peaks_list2 <- peaks_list
  }

  peaks_mat <- do.call(rbind, peaks_list2)
  peaks_df <- as.data.frame(peaks_mat)
  saveRDS(peaks_df, paste0("data-raw/peaks/peaks_sep/peaks_", states[x],
                           ".RDS"))
}

states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "WY", "WA", "AZ", "UT")
# get peaks for all states that were initially missing and now have data
usgs_fs_cl <- readRDS("data-raw/usgs_fs/usgs_fs_miss_corr.RDS")
data.table::setDT(usgs_fs_cl)

for (x in seq_along(states)) {
  usgs_fs <- usgs_fs_cl[state == states[x] & !is.na(discharge) & minpeak < 1]
  data.table::setDT(usgs_fs)
  peaks_list <- vector("list", length = nrow(usgs_fs))

  # begin loop
  rhv_tot <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[x], ".RDS"))
  data.table::setDT(rhv_tot)
  vec <- c()
  for (i in seq_len(nrow(usgs_fs))) {
    temp <- rhv_tot[id == formatC(usgs_fs$site_no[i],
                                  width = 8,
                                  format = "d",
                                  flag = "0")]
    if (nrow(temp) == 0) {
      peaks_list[[i]] <- NA
    } else {
      peaks_list[[i]] <- peakdf(df = temp, minpeak = usgs_fs$minpeak[i])
    }

    if (!inherits(peaks_list[[i]], "data.frame")) {
      vec[i] <- i
    }
  }

  if (!inherits(vec, "NULL")) {
    peaks_list2 <- peaks_list[-vec[which(!is.na(vec))]]
  } else {
    peaks_list2 <- peaks_list
  }

  peaks_mat <- do.call(rbind, peaks_list2)
  peaks_df <- as.data.frame(peaks_mat)
  saveRDS(peaks_df, paste0("data-raw/peaks/peaks_sep/peaks_", states[x],
                           "_miss.RDS"))
}

# combine peaks dfs for each state
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "WY", "WA", "AZ", "UT")
for (i in seq_along(states)) {
  peaks_og <- readRDS(paste0("data-raw/peaks/peaks_sep/peaks_", states[i],
                             ".RDS"))
  peaks_miss <- readRDS(paste0("data-raw/peaks/peaks_sep/peaks_", states[i],
                               "_miss.RDS")
  )
  peaks_tot <- rbind(peaks_og, peaks_miss)
  saveRDS(peaks_tot, paste0("data-raw/peaks/peaks_fin/peaks_fin_", states[i],
                            ".RDS"))
}

# make overall df of peaks for all states
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "WY", "WA", "AZ", "UT")
peaks_tot <- vector("list", length = length(states))
for (i in seq_along(states)) {
  temp <- readRDS(paste0("data-raw/peaks/peaks_fin/peaks_fin_", states[i],
                         ".RDS"))
  add_state <- cbind(rep(states[i], times = nrow(temp)), temp)
  colnames(add_state)[1] <- "state"
  peaks_tot[[i]] <- add_state
}
peaks_totmat <- do.call(rbind, peaks_tot)
peaks_totdf <- as.data.frame(peaks_totmat)
saveRDS(peaks_totdf, "data-raw/peaks/peaks_tot.RDS")
