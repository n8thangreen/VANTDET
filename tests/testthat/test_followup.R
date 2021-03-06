
context("dectree followup times")

source(here::here("scripts/setup.R"))

dectree_test <-
  purrr::partial(.f = dectree,
                 data = data,
                 nsim = 1,
                 name.newtest = "transcriptomic",
                 costDistns = costs,
                 time_res = list(time_res$transcriptomic),
                 drug = drug)


  followup_pdf <- data.frame(x = c(1, 2),
                             y = c(0, 1))

  PERFORMANCE <- list(sens =
                        list(distn = "unif",
                             params = c(min = 0, max = 0)),
                      spec =
                        list(distn = "unif",
                             params = c(min = 0, max = 0)))

  QALY_LOSS <-
    list(TB = list(distn = "unif",
                   params = c(min = 365.25,
                              max = 365.25)),
         Hepatotoxicity = list(distn = "unif",
                               params = c(min = 365.25,
                                          max = 365.25)),
         Nausea = list(distn = "unif",
                       params = c(min = 365.25,
                                  max = 365.25)))


  test_that("dectree followup and QALY loss", {

    out <- dectree_test(
      prevalence = 1,
      performance = list(PERFORMANCE),
      terminal_cost = function(...) c(0, 1, 0,0),
      terminal_health = function(health) c(0, health$followup, 0,0),
      followup_pdf = followup_pdf,
      QALYloss = QALY_LOSS
    )

    expect_equal(unname(out$e[,'e1']), 1)

    out <- dectree_test(
      prevalence = 0,
      performance = list(PERFORMANCE),
      terminal_cost = function(...) c(0,0, 1, 0),
      terminal_health = function(health) c(0,0, health$twomonthTx, 0),
      followup_pdf = followup_pdf,
      QALYloss = QALY_LOSS
    )

    expect_equal(unname(out$e[,'e1']), 3)

  })
