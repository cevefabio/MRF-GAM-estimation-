rm(list = ls())

# Load required libraries for GAM estimation, Excel I/O, and plotting.
library(mgcv)
library(openxlsx)
library(ggplot2)
library(gridExtra)

# Set the project root so relative paths below resolve correctly.
setwd("~/GitHub/MRF-GAM-estimation-")

# Import the main survey dataset used for estimation.
data <- read.xlsx("data/gam_fish_data_v3.xlsx")

# Keep only the first response for each household (unique family id).
data <- data[!duplicated(data$fam_id), ]

# Define VTT (Value of Travel Time) as in Fezzi et al. (2014).
data$VTT <- 0.75 * (data$average_inc / 2000)

# Compute round-trip travel cost using:
# - time cost: VTT multiplied by total travel time in hours,
# - fuel cost: distance in km multiplied by per-km fuel cost.
# Assumptions: 1.5 €/liter fuel price and 0.057 l/km efficiency
# (2019 data from ministero per la transizione ecologica and IEA).
data$tr_cost <- (data$VTT * (data$time_maps * 2 / 3600)) +
  ((data$dist_maps * 2 / 1000) * (1.5 * 0.057))

# Remove clearly invalid age entries caused by data-entry typos.
data <- data[data$age >= 14, ]

######################################################
###### DISTANZE IN TERM OF travel cost ###############
######################################################

## ZERO-INFLATED POISSON GAM SPECIFICATIONS

# Model 4: linear mean equation and linear zero-inflation equation.
trc_m4 <- gam(
  list(
    uscite ~ tr_cost + age + male,
    ~ tr_cost + age + male
  ),
  data = data,
  family = ziplss()
)

# Model 5: log-log specification in both equations.
trc_m5 <- gam(
  list(
    uscite ~ log(tr_cost) + log(age) + male,
    ~ log(tr_cost) + log(age) + male
  ),
  data = data,
  family = ziplss()
)

# Model 6: flexible GAM with cubic regression splines in both equations.
trc_m6 <- gam(
  list(
    uscite ~ s(tr_cost, bs = "cr", k = 5) +
      s(age, bs = "cr", k = 5) + male,
    ~ s(tr_cost, bs = "cr", k = 5) +
      s(age, bs = "cr", k = 5) + male
  ),
  knots = list(0, 2, 3, 4, max(data$tr_cost)),
  data = data,
  family = ziplss(),
  gamma = 1
)

# Quick diagnostic/shape check of smooth terms.
plot.gam(trc_m6)

#####################################################
# Save fitted models for later prediction and plotting scripts.
save("trc_m4", "trc_m5", "trc_m6", file = "modelli")
