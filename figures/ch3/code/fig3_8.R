library(data.table)
library(tidyverse)
library(mgcv)

## load data matrix
peak_data_dt <- readRDS("data-raw/modeling/peak_data_sf_FIXED.rds")
ind <- which(peak_data_dt$swe_av == 0 & is.na(peak_data_dt$snow_dep_av))
peak_data_dt$snow_dep_av[ind] <- 0

# reformat peak data so we have the same variables
data.table::setDT(peak_data_dt)
peak_data <- peak_data_dt[, .(id, dt, mult, huc, med_bf = base_med, lat, lon,
                              ros,
                              snowdep = snow_dep_av,
                              prec_med = prec_max,
                              swe = swe_av, temp = temp_degc_med)]

## load prediction profiles
gam_data <- readRDS("data-raw/modeling/gam_datav3.rds")

## set up formula
form1 <- log(mult) ~
  s(temp) +
  s(snowdep) +
  s(prec_med) +
  s(swe) +
  s(log(med_bf)) +
  s(lat, lon, bs = "sos", k = 25)

form2 <- log(mult) ~
  s(temp) +
  s(prec_med) +
  s(swe) +
  s(log(med_bf)) +
  s(lat, lon, bs = "sos", k = 25)

median_fit <- function(pdata, gdata, formula) {
  # fit gam
  gam_fit <- mgcv::gam(formula, data = pdata)

  # make preds
  preds <- predict(gam_fit, gdata)
  preds_capped <- pmax(preds, 0)

  # add preds to predicted data
  gdata$gam_preds <- exp(preds_capped)

  # calculate ratios of mults
  ratios <- gdata |>
    select(id, huc, gam_preds, ros) |>
    group_by(id) |>
    slice(1:2) |>
    pivot_wider(names_from = ros, values_from = gam_preds) |>
    filter(nonros > 1 & ros > 1) |>
    mutate(mult_ratio = ros / nonros)

  # return median only
  med <- median(ratios$mult_ratio)
  med
}

med_distr <- function(pdata, gdata, formula, reps, inds) {
  replicates <- reps
  meds <- c()
  for (i in 1:replicates) {
    # some observations are repeated (roughly 1/3)
    indices_boot <- sample(inds, size = length(inds),
                           replace = TRUE)

    new_data <- pdata[indices_boot, ]

    meds[i] <- median_fit(new_data, gdata, formula)
  }
  meds
}

indices <- 1:nrow(peak_data)

# get medians for both formulas
# takes a while to run
set.seed(90210)
wsd <- med_distr(peak_data, gam_data, form1, 200, indices)
wosd <- med_distr(peak_data, gam_data, form2, 200, indices)

# make data frame
meds_df <- data.frame(wsd, wosd) |>
  pivot_longer(cols = c(wsd, wosd), names_to = "form", values_to = "med")

med_vals <- meds_df |>
  group_by(form) |>
  summarize(median = median(med))


png("figures/ch3/output_fig/fig3_8.png", width = 6, height = 5, units = "in",
    res = 600)
meds_df |>
  ggplot() +
  stat_density(aes(x = med, col = form), geom = "line",
               position = "identity", lwd = 1) +
  geom_vline(data = med_vals,
             aes(xintercept = median, col = form),
             lty = "dashed",
             show.legend = FALSE) +
  annotate("text", x = med_vals$median[1] - .08, y = 10.6, col = "orange",
           label = paste0("median = ", round(med_vals$median[1], 2)),
           size = 3.5) +
  annotate("text", x = med_vals$median[2] + .08, y = 5.3, col = "blue",
           label = paste0("median = ", round(med_vals$median[2], 2)),
           size = 3.5) +
  scale_x_continuous(limits = c(0.75, 1.5), breaks = seq(0.75, 1.5, 0.25)) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 3)) +
  scale_color_manual(values = rev(c("blue", "orange")),
                     labels = rev(c("With Snow Depth",
                                    "Without Snow Depth"))) +
  guides(color = guide_legend(override.aes = list(lty = c(1, 1),
                                                  lwd = c(1, 1)))) +
  xlab("Median ROSSR") +
  ylab("Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10),
        legend.position = c(0.8, 0.905),
        legend.background = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key = element_blank())
dev.off()
