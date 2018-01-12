#
# VANTDET
# Tx_params.R
# treatment costs and quantities


drug <-
  list(rifamicin =
         list(cost_per_batch = 48,
              dose_mg_day = 600,
              pill_per_batch = 100,
              dose_per_pill = 300),
       isoniazid =
         list(cost_per_batch = 19.24,
              dose_mg_day = 300,
              pill_per_batch = 56,
              dose_per_pill = 50),
       pyrazinamid =
         list(cost_per_batch = 38.34,
              dose_mg_day = 2000,
              pill_per_batch = 30,
              dose_per_pill = 500),
       ethambutol =
         list(cost_per_batch = 42.74,
              dose_mg_day = 15,
              pill_per_batch = 56,
              dose_per_pill = 400))

save(drug, file = "data/drug.RData")


# Appendix I: Imperial College – LTBI treatment NICE report
QALYloss <-
  list(TB = list(distn = "gamma",
                 params = c(shape = 5.427,
                            scale = 0.0154)),
       Hepatotoxicity = list(distn = "gamma",
                             params = c(shape = 65.753,
                                        scale = 7e-5)),
       Nausea = list(distn = "gamma",
                     params = c(shape = 109.67,
                                scale = 3e-6)))

save(QALYloss, file = "data/QALYloss.RData")
