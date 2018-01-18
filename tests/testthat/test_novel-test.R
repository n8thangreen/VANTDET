
context("novel test")

source(here::here("scripts/setup.R"))

costs$null_test <- list(distn = 'none',
                        params = c(mean = 0))

dectree_test <-
  purrr::partial(...f = dectree,
                 data = data,
                 nsim = 1,
                 name.newtest = "null_test",
                 costDistns = costs,
                 time_res = list(time_res$proteomic_flowassay),
                 drug = drug,
                 QALYloss = QALYloss)


test_that("dectree status-quo", {

  out <- dectree_test(
    performance = list(performance$proteomic_flowassay),
    terminal_cost = function(cost) c(cost$std.TB,
                                     cost$std.TB,
                                     cost$std.nonTB,
                                     cost$std.nonTB),
    terminal_health = function(health) c(health$std.TB,
                                         health$std.TB,
                                         health$std.nonTB,
                                         health$std.nonTB))

  expect_equal(unname(out$e[,'e1']), unname(out$e[,'e0']))
  expect_equal(unname(out$c[,'c1']), unname(out$c[,'c0']))
})
