#
# VANTDET
# define-decision-tree.R
#


# rule-out test

terminal_cost_ruleout <- function(cost) {

  c(cost$visit1 + cost$std.TB + cost$newtest,
    cost$visit1 + cost$std.TB + cost$newtest + cost$visit2,
    cost$visit1 + cost$std.nonTB + cost$newtest,
    cost$visit1 + cost$newtest)
}

terminal_health_ruleout <- function(health) {

  c(health$std.TB + health$newtest,
    health$std.TB + health$newtest + health$followup,
    health$std.nonTB + health$newtest,
    health$newtest)
}

# rule-in test

terminal_cost_rulein <- function(cost) {

  c(cost$visit1 + cost$newtest,
    cost$visit1 + cost$std.TB + cost$newtest,
    cost$visit1 + cost$twomonthTx + cost$newtest + cost$visit2,
    cost$visit1 + cost$std.nonTB + cost$newtest)
}

terminal_health_rulein <- function(health) {

  c(health$newtest,
    health$std.TB + health$newtest,
    health$twomonthTx + health$newtest,
    health$std.nonTB + health$newtest)
}
