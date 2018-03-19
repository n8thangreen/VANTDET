
#' Decision Tree Calculation
#'
#' Probabilistically incorporates sampling variability
#' (and standard pathway cost and time to diagnosis).
#'
#' @param data IDEA study data (data.frame)
#' @param nsim Number of sample points (integer) Default: 1000
#' @param costDistns List of distribution names and parameter values for each test/procedure
#' @param time_res time to obtain novel test result (list)
#' @param drug treatment drug cost and quantities (list)
#' @param prevalence TB proportion in cohort (double 0-1) Default: 0.25
#' @param performance Sensitivity and specificity test (list)
#' @param c.newtest Rule-out test unit cost (double) Defalut: 0
#' @param name.newtest Name of rule-out test to get at distribution (string)
#' @param quant Quantile value of time to diagnosis and costs from observed data (double 0-1) Default: 0.5 (median)
#' @param QALYloss QALY loss due to active TB (list)
#' @param N Number of patients. The number in the data is used as default
#' @param wholecohortstats Should the output stats be the total or per patient
#' @param terminal_cost function of terminal node total costs
#' @param terminal_health function of terminal node total QALY loss
#' @param followup_pdf Follow-up appointment time probability distribution function
#' @param ...
#'
#' @return Health and cost realisations (list)
#'
#' @examples
#'
dectree <- function(data,
                    nsim = 1000,
                    costDistns,
                    time_res,
                    drug = drug,
                    prevalence = 0.25,
                    performance,
                    c.newtest = 0,
                    name.newtest = NA,
                    quant = 0.5, #median
                    QALYloss,
                    N = nrow(data),
                    wholecohortstats = FALSE,
                    terminal_health,
                    terminal_cost,
                    followup_pdf, ...) {

  stopifnot(name.newtest %in% c(NA, names(costDistns)))
  stopifnot(quant >= 0, quant <= 1)
  stopifnot(nsim > 0,
            prevalence >= 0,
            prevalence <= 1,
            c.newtest >= 0)
  stopifnot(length(performance) == length(name.newtest))
  stopifnot(length(time_res) == length(name.newtest))

  e <- c <- NULL

  num_tests <- length(name.newtest)

  if (wholecohortstats) N <- 1

  # 2 month follow-up appointment times
  if (missing(followup_pdf)) {
    followup_pdf  <-
      as.numeric(data$start.to.FU) %>%
      subset(data$VisitFU == "2 month FU" &
               data$DosanjhGrouped %in% c(1,2) &
               !is.na(data$start.to.FU) &
               data$start.to.FU <= 200 &
               data$start.to.FU > 0) %>%
      density(from = 0, bw = 10)
  }

  followup_cdf <- data.frame(x = followup_pdf$x,
                             y = cumsum(followup_pdf$y)/sum(followup_pdf$y))

  #################
  # sample params #
  #################

  # exclude generic test costs
  costDistns$PET$params["mean"] <- 0
  costDistns$MRI$params["mean"] <- 0
  costDistns$CT$params["mean"]  <- 0

  cost <- health <- list()

  if ("PatientWgt" %in% names(data)) {
    weight <- mean(data$PatientWgt, na.rm = TRUE)
  }else{
    weight <- 67.98}  #kg

  treatment.days <- 60
  cost$twomonthTx <- treatment.days * (total_drug_cost(drug$rifamicin) +
                                         total_drug_cost(drug$isoniazid) +
                                         total_drug_cost(drug$pyrazinamid) +
                                         total_drug_cost(drug$ethambutol, weight))

  for (i in seq_len(nsim)) {

    rperformance <- lapply(performance, treeSimR::sample_distributions)
    rcosts <- treeSimR::sample_distributions(costDistns)
    t.newtest <- lapply(time_res, treeSimR::sample_distributions)
    rQALYloss <- treeSimR::sample_distributions(QALYloss)/365.25

    rfollowup <- inverse_sample(followup_cdf)

    # respiratory medicine, multi-professional (National tariff)
    cost$visit1 <- rcosts$visit1
    cost$visit2 <- rcosts$visit2

    totalcost <- calcPatientCostofTests(data, COSTS = rcosts)

    whoCat4Treated <- !is.na(data$TBDrugStart.min) & is_notTB(data)

    totalcost[whoCat4Treated] <- totalcost[whoCat4Treated] + cost$twomonthTx

    if (!all(is.na(name.newtest))) cost$newtest <- unlist(rcosts[name.newtest])

    health$newtest <- rQALYloss$TB * unlist(t.newtest)

    health$followup <- rQALYloss$TB * rfollowup

    twomonthTx_QALYloss <- rQALYloss[c('TB', 'Hepatotoxicity', 'Nausea')] %>% sum
    health$twomonthTx <- twomonthTx_QALYloss * rfollowup

    # current observed time and cost estimates
    # balanced samples
    sboot.nonTB <-
      is_notTB(data) %>%
      which() %>%
      sample(replace = TRUE)

    sboot.TB <-
      is_TB(data) %>%
      which() %>%
      sample(replace = TRUE)

    cost$std.nonTB <- median(totalcost[sboot.nonTB])
    cost$std.TB <- median(totalcost[sboot.TB])

    start.to.diag.nonTB <- median(data$start.to.diag[sboot.nonTB])
    start.to.diag.TB <- median(data$start.to.diag[sboot.TB])

    health$std.TB <- rQALYloss$TB * start.to.diag.TB
    health$std.nonTB <- rQALYloss$TB * start.to.diag.nonTB

    ################
    # indiv sample #
    ################

    ##TODO: generalise to any number of test and not just rulein test

    TB  <- rbinom(n = 1, size = N, prob = prevalence)
    nTB <- N - TB

    TBpos <- rbinom(n = 1,
                    size = TB,
                    prob = rperformance[[1]]$sens)
    TBneg <- TB - TBpos

    nTBpos <- rbinom(n = 1,
                     size = nTB,
                     prob = 1 - rperformance[[1]]$spec)
    nTBneg <- nTB - nTBpos

    # final subpopulation sizes
    pop_std <- c(TB, nTB)
    pop <- c(TBpos, TBneg, nTBpos, nTBneg)

    if (num_tests == 2) {

      TBpos_dualpos <- rbinom(n = 1,
                              size = TBpos,
                              prob = rperformance[[2]]$sens)
      TBpos_dualneg <- TBpos - TBpos_dualpos

      nTBpos_dualpos <- rbinom(n = 1,
                               size = nTBpos,
                               prob = 1 - rperformance[[2]]$spec)
      nTBpos_dualneg <- nTBpos - nTBpos_dualpos

      pop <- c(TBpos_dualpos, TBpos_dualneg, TBneg, nTBpos_dualpos, nTBpos_dualneg, TBneg)
    }

    ##########
    # totals #
    ##########

    terminal_cost_std <- c(cost$std.TB,
                           cost$std.nonTB)

    terminal_health_std <- c(health$std.TB,
                             health$std.nonTB)

    Ec.old <- (pop_std %*% terminal_cost_std)/N
    Ee.old <- (pop_std %*% terminal_health_std)/N

    #THIS IS A BIT OF A PROBLEM FOR VARYING PREVALENCE IN ORDER TO BE A COMPARISON FOR ALL...
    #SIMILARLY THE SAMPLED CURRENT TIMES AND COSTS

    if (!is.na(Ec.old) & !is.na(Ee.old)) {

      e <- rbind(e,
                 c(Ee.old, (pop %*% terminal_health(health))/N))
      c <- rbind(c,
                 c(Ec.old, (pop %*% terminal_cost(cost))/N))
    }
  }

  colnames(e) <- c('e0', 'e1')
  colnames(c) <- c('c0', 'c1')

  res <- list(e = e,
              c = c)

  class(res) <- append(class(res), "dectree")

  return(res)
}


