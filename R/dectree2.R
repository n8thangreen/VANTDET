
#' Decision Tree Calculation
#'
#' Probabilistically incorporates sampling variability
#' (and standard pathway cost and time to diagnosis).
#'
#' @param data IDEA study data (data.frame)
#' @param nsim Number of sample points (integer) Default: 1000
#' @param costDistns List of distribution names and parameter values for each test/procedure
#' @param time_res time to obtain novel test result (list)
#' @param prevalence TB proportion in cohort (double 0-1) Default: 0.25
#' @param cutoff Clinical judgement threshold (double 0-1) Default: 1
#' @param FNtime False negative follow-up time fixed value (double) Default: 42
#' @param FNdist Should false negative time to follow-up distribution be used (logical) Default: TRUE
#' @param performance Sensitivity and specificity test (list)
#' @param c.ruleout Rule-out test unit cost (double) Defalut: 0
#' @param name.ruleout Name of rule-out test to get at distribution (string)
#' @param quant Quantile value of time to diagnosis and costs from observed data (double 0-1) Default: 0.5 (median)
#' @param utility QALY adjustment utility due to active TB
#' @param N Number of patients. The number in the data is used as default
#' @param wholecohortstats Should the output stats be the total or per patient
#' @param terminal_cost function of terminal node total costs
#' @param terminal_health function of terminal node total QALY loss
#'
#' @return Health and cost realisations (list)
#'
#' @examples
#'
dectree2 <- function(data,
                     nsim = 100,
                     costDistns,
                     time_res,
                     prevalence = 0.25,
                     cutoff = 1, #ie no clinical judgement
                     FNtime = 42,
                     FNdist = TRUE,
                     performance,
                     c.ruleout = 0,
                     name.ruleout = NA,
                     quant = 0.5, #median
                     utility = NA,
                     N = nrow(data),
                     wholecohortstats = FALSE,
                     terminal_health,
                     terminal_cost) {

  # require(assertive)
  require(triangle)

  stopifnot(name.ruleout %in% c(NA, names(costDistns)))
  stopifnot(quant >= 0, quant <= 1)
  stopifnot(nsim > 0,
            prevalence >= 0,
            prevalence <= 1,
            c.ruleout >= 0,
            cutoff >= 0,
            cutoff <= 1,
            FNtime >= 0)

  median <- purrr::partial(...f = quantile, probs = quant, na.rm = TRUE)

  e <- c <- NULL

  if (wholecohortstats) N <- 1

  # 2 month follow-up appointment times
  FNdens <-
    as.numeric(data$start.to.FU) %>%
    subset(data$VisitFU == "2 month FU" &
             data$DosanjhGrouped %in% c(1,2) &
             !is.na(data$start.to.FU) &
             data$start.to.FU <= 200 &
             data$start.to.FU > 0) %>%
    density(from = 0, bw = 10)

  Fx <- cumsum(FNdens$y)/sum(FNdens$y)


  #################
  # sample params #
  #################

  if (is.na(utility)) {
    # QoL detriment: triangle(0.11, 0.21, 0.31)
    utility <- rtriangle(n = nsim, a = 0.69, b = 0.89)  #1-QALY loss i.e. relative to non-TB (<1)
  }else{
    utility <- rep(utility, time = nsim)}

  ## don't include generic tests costs
  costDistns$PET$params["mean"] <- 0
  costDistns$MRI$params["mean"] <- 0
  costDistns$CT$params["mean"]  <- 0

  cost <- health <- list()

  for (i in 1:nsim) {

    # respiratory medicine, multi-professional (National tariff)
    cost$visit1 <- rgamma(n = 1, shape = 53.3, scale = 4.52)
    cost$visit2 <- rgamma(n = 1, shape = 18.78, scale = 7.62)

    rperformance <- treeSimR::sample_distributions(performance)
    rcosts <- treeSimR::sample_distributions(costDistns)
    t.ruleout <- unlist(treeSimR::sample_distributions(time_res))

    totalcost <- calcPatientCostofTests(data, COSTS = rcosts)

    if ("PatientWgt" %in% names(data)) {
      weight <- mean(data$PatientWgt, na.rm = TRUE)
    }else{
      weight <- 67.98}  #kg

    whoCat4Treated <- !is.na(data$TBDrugStart.min) & data$DosanjhGrouped == "4"

    treatment.days <- 60
    twomonthTreatCost <- treatment.days * ((rifamicin.cost_qty*rifamicin.mg_day)/(rifamicin.pill_qty*rifamicin.mg_pill) +
                                             (isoniazid.cost_qty*isoniazid.mg_day)/(isoniazid.pill_qty*isoniazid.mg_pill) +
                                             (pyrazinamide.cost_qty*pyrazinamide.mg_day)/(pyrazinamide.pill_qty*pyrazinamide.mg_pill) +
                                             (ethambutol.cost_qty*ethambutol.mg_day_kg*weight)/(ethambutol.pill_qty*ethambutol.mg_pill))

    totalcost[whoCat4Treated] <- totalcost[whoCat4Treated] + twomonthTreatCost

    if (!is.na(name.ruleout)) cost$ruleout <- rcosts[[name.ruleout]]

    health$ruleout <- utility[i] * t.ruleout

    if (FNdist) {
      health$FN <- utility[i] * FNdens$x[sum(runif(1) > Fx)]
    }else{
      health$FN <- utility[i]*FNtime}

    TB  <- rbinom(n = 1, size = N, prob = prevalence)
    nTB <- N - TB

    # where are these numbers from?
    TBhighrisk <- rbinom(n = 1, size = TB, prob = 1 - pbeta(cutoff, 7, 3))  # clinical positive diagnosis #dont currently use this
    TBlowrisk  <- TB - TBhighrisk

    nTBhighrisk <- rbinom(n = 1, size = nTB, prob = 1 - pbeta(cutoff, 3, 7))
    nTBlowrisk  <- nTB - nTBhighrisk

    TBpos <- rbinom(n = 1, size = TBlowrisk, prob = rperformance$sens)
    TBneg <- TBlowrisk - TBpos

    nTBpos <- rbinom(n = 1, size = nTBlowrisk, prob = 1 - rperformance$spec)
    nTBneg <- nTBlowrisk - nTBpos

    # final subpopulation sizes
    pop <- c(TBhighrisk, nTBhighrisk, TBpos, TBneg, nTBpos, nTBneg)

    # current observed time and cost estimates
    sboot.nonTB <- sample(which(data$DosanjhGrouped == 4), replace = TRUE)
    sboot.TB <- sample(which(data$DosanjhGrouped %in% c(1,2,3)), replace = TRUE)

    cost$std.nonTB <- median(totalcost[sboot.nonTB]) %>% unname()
    cost$std.TB <- median(totalcost[sboot.TB]) %>% unname()
    start.to.diag.nonTB <- median(data$start.to.diag[sboot.nonTB]) %>% unname()
    start.to.diag.TB <- median(data$start.to.diag[sboot.TB]) %>% unname()

    health$std.TB <- utility[i]*start.to.diag.TB
    health$std.nonTB <- utility[i]*start.to.diag.nonTB

    ##########
    # totals #
    ##########

    Ec.old <- (cost$visit1*N + cost$std.TB*TB + cost$std.nonTB*nTB)/N     #THIS IS A BIT OF A PROBLEM FOR VARYING PREVALENCE IN ORDER TO BE A COMPARISON FOR ALL...
    Ee.old <- (health$std.TB*TB + health$std.nonTB*nTB)/N                 #SIMILARLY THE SAMPLED CURRENT TIMES AND COSTS


    ## expected values
    if (!is.na(Ec.old) & !is.na(Ee.old)) {

      e <- rbind(e,
                 c(Ee.old, (pop %*% terminal_health(health))/N))
      c <- rbind(c,
                 c(Ec.old, (pop %*% terminal_cost(cost))/N))
    }

  }

  colnames(e) <- c('e0', 'e1')
  colnames(c) <- c('c0', 'c1')

  return(list(e = e,
              c = c))
}


