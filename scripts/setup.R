#
# VANTDET
# setup.R
# N Green


devtools::load_all(".")

library(IDEAdectree)
library(BCEA)
library(ggplot2)
library(purrr)
library(dplyr)
library(formattable)
library(plotCostEffectiveness)


data("TBdata_clinical_cleaned", package = "IDEAdectree")

data("drug", package = "VANTDET")
data("costs", package = "VANTDET")
data("QALYloss",  package = "VANTDET")
data("time_res", package = "VANTDET")
data("performance", package = "VANTDET")

dat <- list()

QUANT <- 0.5  #0.65
yearindays <- 365.25
WTP <- c(20000, 30000)/yearindays

