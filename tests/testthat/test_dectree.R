
context("dectree i/o")

source(here::here("scripts/setup.R"))

dectree_test <-
  purrr::partial(.f = dectree,
                 data = data,
                 nsim = 1,
                 name.newtest = "transcriptomic",
                 costDistns = costs,
                 time_res = list(time_res$transcriptomic),
                 drug = drug,
                 QALYloss = QALYloss)


test_that("dectree performance and prevalence", {

  # perfect test
  PERFORMANCE <- list(list(
    sens =
      list(distn = "unif",
           params = c(min = 1, max = 1)),
    spec =
      list(distn = "unif",
           params = c(min = 1, max = 1))))

  out <- dectree_test(
    prevalence = 1,
    performance = PERFORMANCE,
    terminal_cost = function(...) c(1,0,0,0),
    terminal_health = function(...) c(1,0,0,0))

  expect_equal(unname(out$e[,'e1']), 1)
  expect_equal(unname(out$c[,'c1']), 1)

  out <- dectree_test(
    prevalence = 1,
    performance = PERFORMANCE,
    terminal_cost = function(...) c(0,1,1,1),
    terminal_health = function(...) c(0,1,1,1))

  expect_equal(unname(out$e[,'e1']), 0)
  expect_equal(unname(out$c[,'c1']), 0)

  out <- dectree_test(
    prevalence = 0,
    performance = PERFORMANCE,
    terminal_cost = function(...) c(0,0,0,1),
    terminal_health = function(...) c(0,0,0,1))

  expect_equal(unname(out$e[,'e1']), 1)
  expect_equal(unname(out$c[,'c1']), 1)

  out <- dectree_test(
    prevalence = 0,
    performance = PERFORMANCE,
    terminal_cost = function(...) c(1,1,1,0),
    terminal_health = function(...) c(1,1,1,0))

  expect_equal(unname(out$e[,'e1']), 0)
  expect_equal(unname(out$c[,'c1']), 0)
})


test_that("dectree terminal cost/health and prevalence", {

  out <- dectree_test(
    prevalence = 1,
    performance = list(performance$transcriptomic_rulein),
    terminal_cost = function(...) c(0,0,1,1),
    terminal_health = function(...) c(0,0,1,1))

  expect_equal(unname(out$e[,'e1']), 0)
  expect_equal(unname(out$c[,'c1']), 0)

  out <- dectree_test(
    prevalence = 1,
    performance = list(performance$transcriptomic_rulein),
    terminal_cost = function(...) c(1,1,0,0),
    terminal_health = function(...) c(1,1,0,0))

  expect_equal(unname(out$e[,'e1']), 1)
  expect_equal(unname(out$c[,'c1']), 1)

  out <- dectree_test(
    prevalence = 0,
    performance = list(performance$transcriptomic_rulein),
    terminal_cost = function(...) c(1,1,0,0),
    terminal_health = function(...) c(1,1,0,0))

  expect_equal(unname(out$e[,'e1']), 0)
  expect_equal(unname(out$c[,'c1']), 0)

  out <- dectree_test(
    prevalence = 0,
    performance = list(performance$transcriptomic_rulein),
    terminal_cost = function(...) c(0,0,1,1),
    terminal_health = function(...) c(0,0,1,1))

  expect_equal(unname(out$e[,'e1']), 1)
  expect_equal(unname(out$c[,'c1']), 1)

  out <- dectree_test(
    prevalence = 0.5,
    performance = list(performance$transcriptomic_rulein),
    terminal_cost = function(...) c(1,1,1,1),
    terminal_health = function(...) c(1,1,1,1))

  expect_equal(unname(out$e[,'e1']), 1)
  expect_equal(unname(out$c[,'c1']), 1)

  out <- dectree_test(
    prevalence = 0.5,
    performance = list(performance$transcriptomic_rulein),
    terminal_cost = function(...) c(0,0,0,0),
    terminal_health = function(...) c(0,0,0,0))

  expect_equal(unname(out$e[,'e1']), 0)
  expect_equal(unname(out$c[,'c1']), 0)

})

