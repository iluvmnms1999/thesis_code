## peak detection function
peakdf <- function(df, minpeak) {
  peakspor <- cardidates::peakwindow(df$datetime, df$max_flow, minpeak = minpeak
  )
  peakspordf <- peakspor[[1]]
  dt <- as.POSIXct(peakspor[[1]][, 3], "US/Pacific", origin = "1970-01-01")
  peakspordf$dt <- dt
  peakspordf$id <- rep(df$id[1], nrow(peakspordf))
  peakspordf[, c(7, 6, 5)]
}

