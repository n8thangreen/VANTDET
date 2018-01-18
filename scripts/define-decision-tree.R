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

#
##
##TODO:
##
#


terminal_cost_dual <- function(cost) {

  c(cost$visit1 + cost$first_test + cost$second_test,
    cost$visit1 + cost$std.TB + cost$first_test,
    cost$visit1 + cost$twomonthTx + cost$first_test + cost$second_test + cost$visit2,
    cost$visit1 + cost$std.nonTB + cost$first_test)
}

terminal_health_dual <- function(health) {

  c(health$first_test + health$second_test,
    health$std.TB + health$first_test,
    health$twomonthTx + health$first_test + health$second_test,
    health$std.nonTB + health$first_test)
}



