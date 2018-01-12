#
# VANTDET: create input parameter lists
# new_test_params.R
# N Green
#

##TODO: by HIV status?
# check prevalence


#########
# costs #
#########

# 8 hour staff time @ RA:
# £24.20 * 8 = 193.6
# http://www.imperial.ac.uk/media/imperial-college/administration-and-support-services/hr/public/salaries/job-families/AR---London-SP-Rates---2017-18.pdf

costs <- COST.distns.allerror

costs$transcript$distn <- "none"
costs$transcript$params <- c(mean = 16.5 + 193.6)

costs$proteomic_SELDI$distn <- "none"
costs$proteomic_SELDI$params <- c(mean = 36 + 193.6)

costs$proteomic_LC_MS$distn <- "none"
costs$proteomic_LC_MS$params <- c(mean = 150 + 193.6)

costs$proteomic_ELISA$distn <- "none"
costs$proteomic_ELISA$params <- c(mean = 20 + 193.6)

costs$proteomic_flowassay$distn <- "none"
costs$proteomic_flowassay$params <- c(mean = 100 + 150)

##TODO:
costs$molecular$distn <- "none"
costs$molecular$params <- c(mean = NA)

costs$visit1$distn <- "gamma"
costs$visit1$params <- c(shape = 53.3, scale = 4.52)

costs$visit2$distn <- "gamma"
costs$visit2$params <- c(shape = 18.78, scale = 7.62)


#########
# times #
#########

time_res <- list()

time_res$transcript <- list(distn = "unif",
                            params = c(min = 0.5, max = 2)) #12-48 hours

time_res$proteomic_SELDI <- list(distn = "unif",
                                 params = c(min = 0.5, max = 2))

time_res$proteomic_LC_MS <- list(distn = "unif",
                                 params = c(min = 0.5, max = 2))

time_res$proteomic_ELISA <- list(distn = "unif",
                                 params = c(min = 0.5, max = 2))

time_res$proteomic_flowassay <- list(distn = "unif",
                                     params = c(min = 2, max = 7))

##TODO:
time_res$molecular <- list(distn = "unif",
                           params = c(min = NA, max = NA))


####################
# test performance #
####################

performance <- list()

performance$transcript$sens$distn <- "unif"
performance$transcript$sens$params <- c(min = 0.63, max = 0.84)

performance$transcript$spec$distn <- "unif"
performance$transcript$spec$params <- c(min = 0.72, max = 0.89)

##TODO:
performance$proteomic_SELDI$sens$distn <- "unif"
performance$proteomic_SELDI$sens$params <- c(min = NA, max = NA)

performance$proteomic_SELDI$spec$distn <- "unif"
performance$proteomic_SELDI$spec$params <- c(min = NA, max = NA)

performance$proteomic_LC_MS$sens$distn <- "unif"
performance$proteomic_LC_MS$sens$params <- c(min = 0.55, max = 0.91)

performance$proteomic_LC_MS$spec$distn <- "unif"
performance$proteomic_LC_MS$spec$params <- c(min = 0.63, max = 0.93)

performance$proteomic_ELISA$sens$distn <- "unif"
performance$proteomic_ELISA$sens$params <- c(min = NA, max = NA)

performance$proteomic_ELISA$spec$distn <- "unif"
performance$proteomic_ELISA$spec$params <- c(min = NA, max = NA)


performance$proteomic_flowassay$sens$distn <- "unif"
performance$proteomic_flowassay$sens$params <- c(min = 0.5487, max = 0.9064)

performance$proteomic_flowassay$spec$distn <- "unif"
performance$proteomic_flowassay$spec$params <- c(min = 0.718, max = 0.966)

##TODO:
performance$molecular$sens$distn <- "unif"
performance$molecular$sens$params <- c(min = NA, max = NA)

performance$molecular$spec$distn <- "unif"
performance$molecular$spec$params <- c(min = NA, max = NA)


# save --------------------------------------------------------------------

save(costs, file = "data/costs.RData")
save(time_res, file = "data/time_res.RData")
save(performance, file = "data/performance.RData")

