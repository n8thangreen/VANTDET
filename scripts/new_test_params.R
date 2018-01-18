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

# load("C:/Users/ngreen1/Dropbox/TB/IDEA/R/packages/IDEAdectree/data/COSTdistns_allerror.RData")
costs <- COST.distns.allerror

costs$transcript <- list(distn = "none",
                         params = c(mean = 16.5 + 193.6))

costs$proteomic_SELDI <- list(distn = "none",
                              params = c(mean = 36 + 193.6))

# costs$proteomic_LC_MS <- list(distn = "none",
#                               params = c(mean = 150 + 193.6))
#
# costs$proteomic_ELISA <- list(distn = "none",
#                               params = c(mean = 20 + 193.6))

costs$proteomic_flowassay <- list(distn = "none",
                                  params = c(mean = 100 + 150))

##TODO:
costs$molecular <- list(distn = "none",
                        params = c(mean = NA))

costs$visit1 <- list(distn = "gamma",
                     params = c(shape = 53.3, scale = 4.52))

costs$visit2 <- list(distn = "gamma",
                     params = c(shape = 18.78, scale = 7.62))


#########
# times #
#########

time_res <- list()

time_res$transcript <- list(distn = "unif",
                            params = c(min = 0.5, max = 2)) #12-48 hours

time_res$proteomic_SELDI <- list(distn = "unif",
                                 params = c(min = 0.5, max = 2))

time_res$proteomic_flowassay <- list(distn = "unif",
                                     params = c(min = 2, max = 7))

##TODO:
time_res$molecular <- list(distn = "unif",
                           params = c(min = NA, max = NA))

##TODO:
time_res$IGRA <- list(distn = "unif",
                      params = c(min = 1, max = 1))


####################
# test performance #
####################

performance <- list(transcript_rulein =
                             list(sens =
                                    list(distn = "unif",
                                         params = c(min = 0.63, max = 0.84)),
                                  spec =
                                    list(distn = "unif",
                                         params = c(min = 0.72, max = 0.89))),
                    transcript_ruleout =
                      list(sens =
                             list(distn = "unif",
                                  params = c(min = 0.63, max = 0.84)),
                           spec =
                             list(distn = "unif",
                                  params = c(min = 0.72, max = 0.89))),
                    proteomic_SELDI_rulein =
                      list(sens =
                             list(distn = "unif",
                                  params = c(min = NA, max = NA)),
                           spec =
                             list(distn = "unif",
                                  params = c(min = NA, max = NA))),
                    proteomic_SELDI_ruleout =
                      list(sens =
                             list(distn = "unif",
                                  params = c(min = NA, max = NA)),
                           spec =
                             list(distn = "unif",
                                  params = c(min = NA, max = NA))))


performance$proteomic_flowassay <- list(sens =
                                          list(distn = "unif",
                                               params = c(min = 0.5487, max = 0.9064)),
                                        spec =
                                          list(distn = "unif",
                                               params = c(min = 0.718, max = 0.966)))

##TODO:
performance$molecular <- list(sens =
                                list(distn = "unif",
                                     params = c(min = NA, max = NA)),
                              spec =
                                list(distn = "unif",
                                     params = c(min = NA, max = NA)))

##TODO:
performance$IGRA <- list(sens =
                           list(distn = "unif",
                                params =  c(min = 0.9, max = 0.9)),
                         spec =
                           list(distn = "unif",
                                params =  c(min = 0.9, max = 0.9)))


# save --------------------------------------------------------------------

save(costs, file = "data/costs.RData")
save(time_res, file = "data/time_res.RData")
save(performance, file = "data/performance.RData")

