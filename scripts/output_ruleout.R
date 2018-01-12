#
# VANTDET: output table of key cost-effectiveness statistics
# output_ruleout.R
# N Green
#

# HIV?

source(file = "scripts/define-decision-tree.R")

dat <- list()

dat[['transcript']] <- dectree(
  data = data,
  name.newtest = "transcript",
  costDistns = costs,
  performance = performance$transcript,
  time_res = time_res$transcript,
  drug = drug,
  QALYloss = QALYloss,
  terminal_cost = terminal_cost_ruleout,
  terminal_health = terminal_health_ruleout)

# dat[['proteomic_SELDI']] <- dectree(
#   data = data,
#   name.newtest = "proteomic_SELDI",
#   costDistns = costs,
#   performance = performance$proteomic_SELDI,
#   time_res = time_res$proteomic_SELDI,
#   drug = drug,
#   QALYloss = QALYloss,
#   terminal_cost = terminal_cost_ruleout,
#   terminal_health = terminal_health_ruleout)

dat[['proteomic_LC_MS']] <- dectree(
  data = data,
  name.newtest = "proteomic_LC_MS",
  costDistns = costs,
  performance = performance$proteomic_LC_MS,
  time_res = time_res$proteomic_LC_MS,
  drug = drug,
  QALYloss = QALYloss,
  terminal_cost = terminal_cost_ruleout,
  terminal_health = terminal_health_ruleout)

# dat[['proteomic_ELISA']] <- dectree(
#   data = data,
#   name.newtest = "proteomic_ELISA",
#   costDistns = costs,
#   performance = performance$proteomic_ELISA,
#   time_res = time_res$proteomic_ELISA,
#   drug = drug,
#   QALYloss = QALYloss,
#   terminal_cost = terminal_cost_ruleout,
#   terminal_health = terminal_health_ruleout)

dat[['proteomic_flowassay']] <- dectree(
  data = data,
  name.newtest = "proteomic_flowassay",
  costDistns = costs,
  performance = performance$proteomic_flowassay,
  time_res = time_res$proteomic_flowassay,
  drug = drug,
  QALYloss = QALYloss,
  terminal_cost = terminal_cost_ruleout,
  terminal_health = terminal_health_ruleout)

# dat[['microscopy']] <- dectree(
#   data = data,
#   name.newtest = "microscopy",
#   costDistns = costs,
#   performance = performance$microscopy,
#   time_res = time_res$microscopy,
#   drug = drug,
#   QALYloss = QALYloss,
#   terminal_cost = terminal_cost_ruleout,
#   terminal_health = terminal_health_ruleout)


e_df <- do.call(cbind, purrr::map(dat, 'e'))[ ,c(1,2,4,6)]
c_df <- do.call(cbind, purrr::map(dat, 'c'))[ ,c(1,2,4,6)]

res_bcea <- bcea(e = e_df,
                 c = -c_df)

contour2(res_bcea, graph = "ggplot2")
my_contour2(res_bcea, graph = "ggplot2", CONTOUR_PC = '50%')
my_contour2_facet(dat)

cost_effectiveness_table(dat)
cost_effectiveness_table(res_bcea)


write.csv(x = result_tab,
          file = "../../../output_data/ICERtable_ruleout.csv")


