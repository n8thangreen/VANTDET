#
# VANTDET:
# output table of key cost-effectiveness statistics for rule-in test
# output_rulein.R
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
  terminal_cost = terminal_cost_rulein,
  terminal_health = terminal_health_rulein)

# dat[['proteomic_SELDI']] <- dectree(
#   data = data,
#   name.newtest = "proteomic_SELDI",
#   costDistns = costs,
#   performance = performance$proteomic_SELDI,
#   time_res = time_res$proteomic_SELDI,
#   drug = drug,
#   QALYloss = QALYloss,
#   terminal_cost = terminal_cost_rulein,
#   terminal_health = terminal_health_rulein)

dat[['proteomic_LC_MS']] <- dectree(
  data = data,
  name.newtest = "proteomic_LC_MS",
  costDistns = costs,
  performance = performance$proteomic_LC_MS,
  time_res = time_res$proteomic_LC_MS,
  drug = drug,
  QALYloss = QALYloss,
  terminal_cost = terminal_cost_rulein,
  terminal_health = terminal_health_rulein)

# dat[['proteomic_ELISA']] <- dectree(
#   data = data,
#   name.newtest = "proteomic_ELISA",
#   costDistns = costs,
#   performance = performance$proteomic_ELISA,
#   time_res = time_res$proteomic_ELISA,
#   drug = drug,
#   QALYloss = QALYloss,
#   terminal_cost = terminal_cost_rulein,
#   terminal_health = terminal_health_rulein)

dat[['proteomic_flowassay']] <- dectree(
  data = data,
  name.newtest = "proteomic_flowassay",
  costDistns = costs,
  performance = performance$proteomic_flowassay,
  time_res = time_res$proteomic_flowassay,
  drug = drug,
  QALYloss = QALYloss,
  terminal_cost = terminal_cost_rulein,
  terminal_health = terminal_health_rulein)

# dat[['microscopy']] <- dectree(
#   data = data,
#   name.newtest = "microscopy",
#   costDistns = costs,
#   performance = performance$microscopy,
#   time_res = time_res$microscopy,
#   drug = drug,
#   QALYloss = QALYloss,
#   terminal_cost = terminal_cost_rulein,
#   terminal_health = terminal_health_rulein)


e_df <- do.call(cbind, purrr::map(dat, 'e'))
c_df <- do.call(cbind, purrr::map(dat, 'c'))

## use a common status-quo sample
# e_df <- e_df[ ,c(1,2,4,6)]
# e_df <- e_df[ ,c(1,2,4,6)]
# res_bcea <- bcea(e = e_df,
#                  c = -c_df,
#                  interventions = c("status-quo", names(dat)))

evens <- seq(from = 2, to = 2*length(dat), 2)
odds <- evens - 1
QALYgain <- as.matrix(data.frame(0, e_df[ ,odds] - e_df[ ,evens]))
cost_incur <- as.matrix(data.frame(0, c_df[ ,evens] - c_df[ ,odds]))

res_bcea <- bcea(e = -QALYgain,
                 c = -cost_incur,
                 interventions = c("status-quo", names(dat)))

contour2(res_bcea, graph = "ggplot2")
my_contour2(res_bcea, graph = "ggplot2", CONTOUR_PC = '5%') + coord_cartesian(xlim = c(-0.01, 0.002)) + theme(legend.position = "none")
my_contour2_facet(dat)

cost_effectiveness_table(dat)
result_tab <- cost_effectiveness_table(res_bcea)


write.csv(x = result_tab,
          file = "output/ICERtable_rulein.csv")


