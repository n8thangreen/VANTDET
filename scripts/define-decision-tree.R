#
# VANTDET
# define-decision-tree.R
#


# rule-out test

terminal_cost_ruleout <- function(cost) {

  c(cost$visit1 + cost$std.TB,
    cost$visit1 + cost$std.nonTB,
    cost$visit1 + cost$std.TB + cost$ruleout,
    cost$visit1 + cost$std.TB + cost$ruleout + cost$visit2,
    cost$visit1 + cost$std.nonTB + cost$ruleout,
    cost$visit1 + cost$ruleout)
}

terminal_health_ruleout <- function(health) {

  c(health$std.TB,
    health$std.nonTB,
    health$std.TB + health$ruleout,
    health$std.TB + health$ruleout + health$FN,
    health$std.nonTB + health$ruleout,
    health$ruleout)
}

# rule-in test

terminal_cost_ruleout <- function(cost) {

  c(cost$visit1 + cost$std.TB,
    cost$visit1 + cost$std.nonTB,
    cost$visit1 + cost$std.TB + cost$ruleout,
    cost$visit1 + cost$std.TB + cost$ruleout + cost$visit2,
    cost$visit1 + cost$std.nonTB + cost$ruleout,
    cost$visit1 + cost$ruleout)
}

terminal_health_ruleout <- function(health) {

  c(health$std.TB,
    health$std.nonTB,
    health$std.TB + health$ruleout,
    health$std.TB + health$ruleout + health$FN,
    health$std.nonTB + health$ruleout,
    health$ruleout)
}
