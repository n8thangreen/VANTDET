
cost_effectiveness_table <- function(dat,
                                     wtp_threshold,
                                     html = TRUE, ...)
  UseMethod("cost_effectiveness_table")


#' cost_effectiveness_table.bcea
#'
#' @param dat
#' @param wtp_threshold
#'
#' @return
#' @export
#'
#' @examples
#'
cost_effectiveness_table.bcea <- function(dat,
                                          wtp_threshold = 20000) {

  out <-
    if (dat$n.comparisons == 1) {
      with(dat,
           do.call(data.frame,
                   list("mean_incr_e" = mean(delta.e),
                        "mean_incr_c" = mean(delta.c),
                        "EINB" = eib[k == wtp_threshold],
                        "percentile_5th" = quantile(x = ib[k == wtp_threshold, ], probs = 0.05),
                        "percentile_95th" = quantile(x = ib[k == wtp_threshold, ], probs = 0.95),
                        "ICER" = ICER,
                        "ceac_WTP15000" = ceac[k == 15000],
                        "ceac_WTP20000" = ceac[k == 20000],
                        "ceac_WTP25000" = ceac[k == 25000],
                        "ceac_WTP30000" = ceac[k == 30000])))
    }else{

      with(dat,
           do.call(data.frame,
                   list("mean_incr_e" = apply(delta.e, 2, mean),
                        "mean_incr_c" = apply(delta.c, 2, mean),
                        "EINB" = eib[k == wtp_threshold, ],
                        "percentile_5th" = apply(ib[k == wtp_threshold, , ], 2, quantile, probs = 0.05),
                        "percentile_95th" = apply(ib[k == wtp_threshold, , ], 2, quantile, probs = 0.95),
                        "ICER" = ICER,
                        "ceac_WTP15000" = ceac[k == 15000, ],
                        "ceac_WTP20000" = ceac[k == 20000, ],
                        "ceac_WTP25000" = ceac[k == 25000, ],
                        "ceac_WTP30000" = ceac[k == 30000, ])))
    }

  return(data.frame('scenario' = dat$interventions[-1], out))
}


#' cost_effectiveness_table.default
#'
#' @param dat cost and health realisations
#' @param html output format
#'
#' @return
#' @export
#'
#' @examples
#'
cost_effectiveness_table.default <- function(dat,
                                             html = TRUE) {

  mean_c1 <- mean_e1 <- vector()
  mean_c0 <- mean_e0 <- vector()
  incr_e <- incr_c <- list()
  mean_incr_e <- mean_incr_c <- vector()
  p_ce2k <- p_ce3k <- vector()
  Eceac2k <- Eceac3k <- vector()

  for (i in 1:length(dat)) {

    mean_c0[i] <- mean(dat[[i]]$c[ ,'c0'])
    mean_e0[i] <- mean(dat[[i]]$e[ ,'e0'])

    mean_c1[i] <- mean(dat[[i]]$c[ ,'c1'])
    mean_e1[i] <- mean(dat[[i]]$e[ ,'e1'])

    incr_e[[i]] <- dat[[i]]$e[ ,'e0'] - dat[[i]]$e[ ,'e1'] #QALY gain
    incr_c[[i]] <- dat[[i]]$c[ ,'c1'] - dat[[i]]$c[ ,'c0'] #cost incurred

    mean_incr_c[i] <- mean(incr_c[[i]])
    mean_incr_e[i] <- mean(incr_e[[i]])

    Eceac2k[i] <-
      mean_incr_c[i] <= (20000 / yearindays * mean_incr_e[i])
    Eceac3k[i] <-
      mean_incr_c[i] <= (30000 / yearindays * mean_incr_e[i])

    p_ce2k[i] <-
      sum(incr_c[[i]] <= (20000 / yearindays * incr_e[[i]])) / length(incr_c[[i]])
    p_ce3k[i] <-
      sum(incr_c[[i]] <= (30000 / yearindays * incr_e[[i]])) / length(incr_c[[i]])
  }

  ICER <- mean_incr_c/mean_incr_e

  result_tab <- data.frame(scenario = names(dat),
                           mean_c0, mean_e0,
                           mean_c1, mean_e1,
                           mean_incr_c, mean_incr_e,
                           ICER,
                           # Eceac2k, Eceac3k,
                           p_ce2k, p_ce3k)

  if (html) {
    sign_formatter <- formatter("span",
                                style = x ~ style(color = ifelse(x > 0.5, "green",
                                                                 ifelse(x < 0.5, "red", "black"))))

    result_tab <- formattable(result_tab, list(
      p_ce2k = sign_formatter,
      p_ce3k = sign_formatter))
  }

  return(result_tab)
}
