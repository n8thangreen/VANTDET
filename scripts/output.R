#
# VANTDET: output table of key cost-effectiveness statistics
# N Green
#

# HIV?


##TODO:
#
xx <- dectree2(
  data = data,
  name.ruleout = "transcript",
  costDistns = costs,
  performance = performance$transcript,
  time_res = time_res$transcript,
  terminal_cost = terminal_cost_ruleout,
  terminal_health = terminal_health_ruleout)


dat <- list()

yy <- dat[['transcript']] <- dectree(
  data = data,
  name.ruleout = "transcript",
  costDistns = costs,
  performance = performance$transcript,
  time_res = time_res$transcript)

# dat[['proteomic_SELDI']] <- dectree(
#   data = data,
#   name.ruleout = "proteomic_SELDI",
#   costDistns = costs,
#   performance = performance$proteomic_SELDI,
#   time_res = time_res$proteomic_SELDI)

dat[['proteomic_LC_MS']] <- dectree(
  data = data,
  name.ruleout = "proteomic_LC_MS",
  costDistns = costs,
  performance = performance$proteomic_LC_MS,
  time_res = time_res$proteomic_LC_MS)

# dat[['proteomic_ELISA']] <- dectree(
#   data = data,
#   name.ruleout = "proteomic_ELISA",
#   costDistns = costs,
#   performance = performance$proteomic_ELISA,
#   time_res = time_res$proteomic_ELISA)

dat[['proteomic_flowassay']] <- dectree(
  data = data,
  name.ruleout = "proteomic_flowassay",
  costDistns = costs,
  performance = performance$proteomic_flowassay,
  time_res = time_res$proteomic_flowassay)

# dat[['microscopy']] <- dectree(
#   data = data,
#   name.ruleout = "microscopy",
#   costDistns = costs,
#   performance = performance$microscopy,
#   time_res = time_res$microscopy)


e_df <- do.call(cbind, map(dat, 'e'))[ ,c(1,2,4,6)]
c_df <- do.call(cbind, map(dat, 'c'))[ ,c(1,2,4,6)]
res_bcea <- bcea(e = e_df/365,
                 c = -c_df)

cost_effectiveness_table(dat)
cost_effectiveness_table(res_bcea)


write.csv(x = result_tab,
          file = "../../../output_data/ICERtable.csv")


