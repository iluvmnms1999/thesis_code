# this script calculates proportion of flood stage discharge to maximum annual
# max flow for each streamgage with a predefined flood stage, takes the median
# of those proportions, and uses the median to estimate this proportion for the
# rest of the gages

library(data.table)

# read in max hourly measurements
states <- c("CA", "CO", "ID", "MT", "NM", "NV", "OR", "WY", "AZ", "WA", "UT")
usgs_fs_cl <- read.csv("data-raw/usgs_fs/usgs_fs_fin.csv", header = TRUE)
data.table::setDT(usgs_fs_cl)

# create minpeak and est indicator columns to fill with loop
usgs_fs_cl <- usgs_fs_cl[, minpeak := rep(0, length = nrow(usgs_fs_cl2))]
usgs_fs_cl <- usgs_fs_cl[, est := is.na(discharge)]

# get overall median for existing fs thresholds
prop_est_lst2 <- vector("list", length = length(states))
for (i in seq_along(states)) {
  rhv_first <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_",
                              states[i], ".RDS")) |>
    setDT()
  rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_",
                             states[i], ".RDS")) |>
    setDT()
  rhv_tot <- rbindlist(list(rhv_first, rhv_miss))
  usgs_fs <- usgs_fs_cl[state == states[i]]

  def <- usgs_fs[est == FALSE]

  # to figure out general trend in proportion for defined thresholds
  prop_est <- c()
  for (j in seq_along(def$site_no)) {
    # subset on station id
    temp <- rhv_tot[id == formatC(def$site_no[j],
                                  width = 8,
                                  format = "d",
                                  flag = "0")]
    # add year to data table
    temp[, year := data.table::year(datetime)]
    # get ann_max
    tib <- temp[, .(ann_max = max(max_flow)), by = year]
    prop <- def$discharge[j] / max(tib$ann_max)
    prop_est[j] <- prop
  }
  # add minpeak props to data frame
  usgs_fs_cl[state == states[i]][est == FALSE]$minpeak <- prop_est
  prop_est_lst2[[i]] <- prop_est
}
ex_props <- unlist(prop_est_lst2)
med <- median(ex_props, na.rm = TRUE)

# use median proportion to estimate flood stages for other stations and add
# estimated minpeak props to data frame
props_lst2 <- vector("list", length = length(states))
for (i in seq_along(states)) {
  rhv_first <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[i],
                              ".RDS")) |>
    setDT()
  rhv_miss <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[i],
                             ".RDS")) |>
    setDT()
  rhv_tot <- rbindlist(list(rhv_first, rhv_miss))
  usgs_fs <- usgs_fs_cl[state == states[i]]

  undef <- usgs_fs[est == TRUE]

  # estimate discharge
  vec <- c()
  for (k in seq_along(undef$site_no)) {
    sub <- rhv_tot[id == formatC(undef$site_no[k],
                                 width = 8,
                                 format = "d",
                                 flag = "0")]
    vec[k] <- round(med * max(sub$max_flow), digits = 2) # using max
  }

  vec[vec < 0] <- NA
  usgs_fs_cl[state == states[i]][est == TRUE]$discharge <- vec
  undef$discharge <- vec

  # estimate minpeak
  props <- c()
  for (x in seq_along(undef$site_no)) {
    sub <- rhv_tot[id == formatC(undef$site_no[x],
                                 width = 8,
                                 format = "d",
                                 flag = "0")]
    props[x] <- undef$discharge[x] / max(sub$max_flow)
  }
  props_lst2[[i]] <- props
  usgs_fs_cl[state == states[i]][est == TRUE]$minpeak <- props
}

# manual minpeak adjustments to match original results from corrmed
ind <- c(525, 527, 566, 584, 587, 601, 607, 631, 634, 636, 727, 813, 814, 1095,
         1271, 1272, 1279, 1510, 1704, 1816, 2001, 2207, 2222, 2229, 2252, 2289,
         2324, 2379, 2405, 2419, 2449, 2458, 2476, 2485)
usgs_fs_cl$minpeak[ind] <- 0

saveRDS(usgs_fs_cl, "data-raw/usgs_fs/usgs_fs_alm.RDS")

# get thresholds for as many missing stations in usgs_fs_alm as possible
usgs_fs_cl4 <- readRDS("data-raw/usgs_fs/usgs_fs_alm.RDS")
data.table::setDT(usgs_fs_cl4)
usgs_fs_miss <- usgs_fs_cl[is.na(discharge)]

props_lst2 <- vector("list", length = length(states))
for (i in seq_along(states)) {
  #rhv_tot <- readRDS(paste0("data-raw/rhv_tot/rhv_tot_", states[i], ".RDS"))
  rhv_tot <- readRDS(paste0("data-raw/rhv_miss/rhv_miss_", states[i], ".RDS"))
  data.table::setDT(rhv_tot)
  usgs_fs <- usgs_fs_miss[state == states[i]]

  vec <- c()
  for (k in seq_along(usgs_fs$discharge)) {
    sub <- rhv_tot[id == formatC(usgs_fs$site_no[k],
                                 width = 8,
                                 format = "d",
                                 flag = "0")]
    if (!is.na(usgs_fs$discharge[k])) {
      vec[k] <- usgs_fs$discharge[k]
    } else {
      # vec[k] <- round(med * max(sub$max_flow[sub$max_flow != max(sub$max_flow)]),
      # digits = 2) # code for finding proportion compared to second highest max
      vec[k] <- round(med * max(sub$max_flow), digits = 2) # using max
    }
  }
  vec[vec < 0] <- NA
  usgs_fs_miss[state == states[i]]$discharge <- vec

  props <- c()
  for (x in seq_along(usgs_fs$site_no)) {
    sub <- rhv_tot[id == formatC(usgs_fs$site_no[x],
                                 width = 8,
                                 format = "d",
                                 flag = "0")]
    props[x] <- usgs_fs$discharge[x] / max(sub$max_flow)
  }
  props_lst2[[i]] <- props
  usgs_fs_miss[state == states[i]]$minpeak <- props
}

usgs_fs_found <- usgs_fs_miss[!is.na(discharge)]

