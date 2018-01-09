#
# VANTDET: set-up script
# N Green
#


devtools::load_all(".")

library(IDEAdectree)
library(BCEA)
library(ggplot2)
library(dplyr)


data("TBdata_clinical_cleaned", package = "IDEAdectree")
data('drug_dose-cost', package = "IDEAdectree")

data("costs", package = "VANTDET")
data("time_res", package = "VANTDET")
data("performance", package = "VANTDET")

date.diff <- datc.diff <- list()
dat.ceac20000 <- dat.ceac30000 <- NA
dat <- list()

QUANT <- 0.5  #0.65
yearindays <- 365
WTP <- c(20000, 30000)/yearindays

