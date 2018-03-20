#
# VANTDET:
# output_rulein.R
# N Green
#
# output table of key cost-effectiveness statistics
# for rule-in test


##TODO: DRY duplication in input arguments


dat <- list()

dat[['transcriptomic']] <- dectree(
  data = data,
  name.newtest = "transcriptomic",
  costDistns = costs,
  performance = list(performance$transcriptomic_rulein),
  time_res = list(time_res$transcriptomic),
  drug = drug,
  QALYloss = QALYloss,
  terminal_cost = terminal_cost_rulein,
  terminal_health = terminal_health_rulein)

dat[['proteomic_SELDI']] <- dectree(
  data = data,
  name.newtest = "proteomic_SELDI",
  costDistns = costs,
  performance = list(performance$proteomic_SELDI_rulein),
  time_res = list(time_res$proteomic_SELDI),
  drug = drug,
  QALYloss = QALYloss,
  terminal_cost = terminal_cost_rulein,
  terminal_health = terminal_health_rulein)


dat[['IGRA_flowcytometry']] <- dectree(
  data = data,
  name.newtest =  c("IGRA", "flow_cytometry"),
  costDistns = costs,
  performance = performance[c('IGRA', 'flow_cytometry_HIVneg')],
  time_res = time_res[c('IGRA', 'flow_cytometry')],
  drug = drug,
  QALYloss = QALYloss,
  terminal_cost = terminal_cost_dual_rulein,
  terminal_health = terminal_health_dual_rulein)


e_df <- do.call(cbind,
                purrr::map(dat, 'e'))
c_df <- do.call(cbind,
                purrr::map(dat, 'c'))

evens <- seq(from = 2, to = 2*length(dat), 2)
odds <- evens - 1

QALYgain <- as.matrix(data.frame(0, e_df[ ,odds] - e_df[ ,evens]))
cost_incur <- as.matrix(data.frame(0, c_df[ ,evens] - c_df[ ,odds]))

res_bcea <- bcea(e = -QALYgain,
                 c = -cost_incur,
                 interventions = c("status-quo", names(dat)))

##########
# output #
##########

contour2(res_bcea, graph = "ggplot2")

my_contour2(res_bcea, graph = "ggplot2", CONTOUR_PC = '5%') +
  coord_cartesian(xlim = c(-0.01, 0.002)) +
  theme(legend.position = "none")

my_contour2_facet(dat)

cost_effectiveness_table(dat)
(result_tab <- cost_effectiveness_table(res_bcea))


write.csv(x = result_tab,
          file = "output/ICERtable_rulein.csv")


