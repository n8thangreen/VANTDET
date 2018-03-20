
context("novel test")

source(here::here("scripts/setup.R"))

dectree_test <-
  purrr::partial(...f = dectree,
                 data = data,
                 nsim = 1,
                 name.newtest = "null_test",
                 time_res = list(time_res$transcriptomic),
                 drug = drug,
                 QALYloss = QALYloss)


test_that("same as dectree status-quo", {

  costs$null_test <- list(distn = 'none',
                          params = c(mean = 0))

  out <- dectree_test(
    performance = list(performance$transcriptomic_rulein),
    costDistns = costs,
    terminal_cost = function(cost) c(cost$newtest + cost$std.TB,
                                     cost$newtest + cost$std.TB,
                                     cost$newtest + cost$std.nonTB,
                                     cost$newtest + cost$std.nonTB),
    terminal_health = function(health) c(health$std.TB,
                                         health$std.TB,
                                         health$std.nonTB,
                                         health$std.nonTB))

  expect_equal(unname(out$e[,'e1']), unname(out$e[,'e0']))
  expect_equal(unname(out$c[,'c1']), unname(out$c[,'c0']))

  # positive value test

  costs$null_test <- list(distn = 'none',
                          params = c(mean = 100))

  out <- dectree_test(
    performance = list(performance$transcriptomic_rulein),
    costDistns = costs,
    terminal_cost = function(cost) c(cost$newtest + cost$std.TB,
                                     cost$newtest + cost$std.TB,
                                     cost$newtest + cost$std.nonTB,
                                     cost$newtest + cost$std.nonTB),
    terminal_health = function(health) c(health$std.TB,
                                         health$std.TB,
                                         health$std.nonTB,
                                         health$std.nonTB))

  expect_equal(unname(out$c[,'c1']) - 100, unname(out$c[,'c0']))
})
