# Supplementary files for "QUANTIFYING THE IMPACT OF RAIN-ON-SNOW INDUCED FLOODING IN THE WESTERN UNITED STATES"

## Authors

**Emma Watts**  
Utah State University  
3900 Old Main Hill  
Logan, UT 84322  
emma.watts@usu.edu  

**Brennan Bean**  
Utah State University  
3900 Old Main Hill  
Logan, UT 84322  
brennan.bean@usu.edu  
ORCiD: [0000-0002-2853-0455](https://orcid.org/0000-0002-2853-0455)  

---

## Abstract

Serious flooding can happen when rain falls on snow, which we call a rain-on-snow (ROS) event. Increasing our understanding of the behavior of floods resulting from ROS events can help us design better systems to manage flood water and prevent it from causing damage. This thesis explores how ROS events affect streamflow in the Western United States by examining the weather conditions that precede a streamflow surge. We classify stream surges as ROS or non-ROS induced based on these weather conditions, which helps us separate floods caused by ROS events from those caused by other factors. By comparing these different types of floods, we find that ROS induced floods are generally 3-20% larger than other floods. Engineers can use these findings and data to conservatively adjust drainage system designs, like culverts, to better handle the increased water flow during ROS events. This will reduce potential damage and make our road and building structures better able to handle extreme flooding.

---

## Details

This repository contains the data and code necessary to reproduce the figures and results in the above-referenced MS Thesis. The main product is `peak_data_sf_FIXED.csv` (use `read.csv2()` to read into R). It also includes the following folders:

### data-raw

This folder contains all the formatted data sources necessary to recreate final figures and results in the repository. These data sources are derived from publicly available snow, weather, and water data available from several federal sources cited in the thesis and referenced in the `scripts` folder.

- **modeling:** All datasets used in the modeling process, for both the GAMs and ML application.
- **peaks:** Streamflow peak data, including corresponding calculated baseflows.
- **rhv_tot/fhv_miss:** Cleaned streamflow data prior to peak detection, with `rhv_tot` containing data downloaded successfully on the first attempt and `rhv_miss` containing data obtained for streamgages that failed to download initially.
- **ros_class:** Streamflow peaks with accompanying ROS vs non-ROS classification.
- **snotel:** Snowpack telemetry station data, used to determine weather conditions preceding a storm surge.
- **usgs_fs:** Streamgage identifier and flood stage data.
- **wbd:** Watershed boundary dataset.

### figures

This folder contains copies of all thesis figures along with the R code necessary to reproduce them. R code is organized by chapter and scripts are named corresponding to figure numbers as they appear in the thesis. Note: a Google API key needs to be obtained and imported in order to run the code in `fig2_11.R`, which includes an overlay over a background map.

### R

This folder contains copies of functions necessary to download the raw data sources. Two of these functions come directly from the rsnodas package (see [rsnodas GitHub repository](https://github.com/lschneider93/rsnodas)).

### scripts

This folder contains the R code required to reproduce the data workflows described in this thesis. Script names include the order in which the scripts should be run. Note that scripts 1-8 all contribute to the creation of the final dataset. These scripts require long data download steps and their reproducibility are conditional upon current server availability as of 8/24/2024 for all publicly available datasets.

---

## Instructions

Full reproducibility requires running the `.R` files in the `scripts` folder in numeric order, based on the script names. Note that computation times listed in the code comments are based on the use of a typical desktop computer with an internet download speed of roughly 1 Gb/sec.

All `.R` files in the `figures` folder are self-contained and reproducible provided that the user has downloaded all necessary R packages and has opened an RStudio IDE session using the `.Rproj` file available in the main project folder.

