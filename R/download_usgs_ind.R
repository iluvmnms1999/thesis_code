# taken directly from rsnodas package

download_usgs <- function(freq = "uv", sites = c("09180000", "09180500"),
                          destpath = paste0(getwd(), "/data-raw/usgs"),
                          begin_date = as.Date("1850-01-01"),
                          end_date = Sys.Date()) {
  # Test to ensure freq arguments are formatted correctly.
  if (!freq %in% c("uv", "dv")) {
    stop("freq argument must be either \"uv\" or \"dv\".")
  }

  # Test to ensure date arguments are formatted correctly.
  if (!inherits(begin_date, c("character", "Date")) |
      !inherits(end_date, c("character", "Date"))) {
    stop("start and end date arguments must be character or date objects.")
  }

  begin_date <- anytime::anydate(begin_date)
  end_date <- anytime::anydate(end_date)

  # Create path if doesn't exist. If it exists, then don't do anything
  dir.create(file.path(destpath), showWarnings = F)

  site_list <- list()
  j <- 1

  for (i in sites) {
    if (nchar(i) < 8) {
      i <- paste0(paste0(rep("0", times = (8 - nchar(i))), collapse = ""), i)
    } else {
      i <- paste0(i)
    }

    url_i <- paste0(
      "https://nwis.waterdata.usgs.gov/usa/nwis/",
      freq,
      "/?cb_00060=on&format=rdb",
      "&site_no=", i,
      "&referred_module=sw",
      "&period=&begin_date=", begin_date,
      "&end_date=", end_date
    )

    content_check <- try(httr::content(httr::GET(url_i)))

    if (inherits(content_check, "character")) {
      if (length(grep("\nUSGS\t", content_check)) > 0) {
        destfile <- paste0(
          destpath, "/site", i
        )

        try_val <- try(download.file(url_i, destfile))

        if (inherits(try_val, "try-error")) {
          try_val <- tryCatch(
            {
              download.file(url_i, destfile, timeout = 500) # changed timeout amount and overrode default with options(timeout = 300)
            },
            error = function(e) {
              cat(
                "Site", i, "failed to download.",
                conditionMessage(e)
              )
            }
          )
        }

        if (!inherits(try_val, "try-error")) {
          tab_if <- try(data.table::fread(file = destfile, header = T)[-1, ]) # added fill

          # probably redundant if statement, but okay for now
          if (nrow(tab_if) != 0) {
            col_len <- length(colnames(tab_if))

            colnames(tab_if)[(col_len - 1):col_len] <- c("v00060", "v00060_cd")

            site_i <- paste0("site", i)
            eval(call("<-", as.name(site_i), tab_if))

            site_list[[j]] <- get(site_i)
            j <- j + 1
          }
        }
      }
    }
  }

  site_tab <- data.table::rbindlist(site_list, use.names = T, fill = T)

  if (freq == "uv" & "datetime" %in% colnames(site_tab)) {
    site_tab <- site_tab %>%
      separate("datetime", c("date", "time"), sep = "\\s")
  }

  site_tab$v00060 <- as.numeric(site_tab$v00060)

  site_tab
}
