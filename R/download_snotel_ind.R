# taken directly from rsnodas package

download_snotel <- function(freq = "hourly",
                            sites = c(
                              "340:NV:SNTL", "615:NV:SNTL",
                              "652:NV:SNTL", "1242:NV:SNTL"
                            ),
                            destpath = paste0(getwd(), "/data"),
                            begin_date = as.Date("1900-01-01"),
                            end_date = Sys.Date()) {

  # Test to ensure freq arguments are formatted correctly.
  if (!freq %in% c("hourly", "daily")) {
    stop("freq argument must be either \"hourly\" or \"daily\".")
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
    site_num <- gsub("[^0-9]", "", i)

    url_i <- paste0(
      "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/",
      "customMultiTimeSeriesGroupByStationReport,metric/",
      freq, # freq
      "/start_of_period/", # baseline
      i, # site
      "%7Cid=%22%22%7Cname/", # baseline
      begin_date, ",", end_date, # dates
      "/TOBS::value,PRCP::value,", # baseline
      "SNWD::value,WTEQ::value,SMS:-2:value,SMS:-8:value,SMS:-20:value",
      "?fitToScreen=false" # baseline
    )

    content_check <- try(httr::content(httr::GET(url_i)))

    if (inherits(content_check, "character")) {
      destfile <- paste0(
        destpath, "/snotel", site_num
      )

      try_val <- try(download.file(url_i, destfile))

      if (inherits(try_val, "try-error")) {
        try_val <- tryCatch(
          {
            download.file(url_i, destfile, timeout = 120)
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
        tab_i <- try(data.table::fread(file = destfile))

        if (!inherits(tab_i, "try-error")) {
          clean_names <- gsub("\\s", "_", colnames(tab_i))
          refine_names <- gsub(paste0(".*", site_num, ")_"), "", clean_names)
          new_names <- gsub("\\(|\\)|-", "", refine_names)
          colnames(tab_i) <- tolower(new_names)

          if (nrow(tab_i) != 0) {
            site_i <- paste0("site", site_num)
            tab_i$id <- i
            tab_i <- tab_i %>% dplyr::relocate(id)
            eval(call("<-", as.name(site_i), tab_i))

            site_list[[j]] <- get(site_i)
            j <- j + 1
          }
        }
      }
    }
  }

  # site_list

  site_tab <- data.table::rbindlist(site_list, use.names = T, fill = T)

  if (freq == "hourly") {
    site_tab <- site_tab %>%
      tidyr::separate("date", c("date", "time"), sep = "\\s")
  }

  site_tab
}
