#
# VANTDET:
# create input parameter lists
# new_test_params.R
# N Green


#########
# costs #
#########

# staff time @ RA:
# http://www.imperial.ac.uk/media/imperial-college/administration-and-support-services/hr/public/salaries/job-families/AR---London-SP-Rates---2017-18.pdf

staff_per_hr <- 24.20
IGRA_staff_hrs <- list(distn = "unif",
                       params = c(min = 6, max = 8)) #0.25-0.33 days

# load("C:/Users/ngreen1/Dropbox/TB/IDEA/R/packages/IDEAdectree/data/COSTdistns_allerror.RData")
data("COSTdistns_allerror", package = "IDEAdectree")
costs <- COST.distns.allerror


costs$transcriptomic <-
  IGRA_staff_hrs %>%
  modify_at(.at = "params",
            .f = function(x) x*staff_per_hr + 16.5)

costs$proteomic_SELDI <-
  IGRA_staff_hrs %>%
  modify_at(.at = "params",
            .f = function(x) x*staff_per_hr + 36)

costs$flow_cytometry <-
  IGRA_staff_hrs %>%
  modify_at(.at = "params",
            .f = function(x) x*staff_per_hr + 150) #55-350

##TODO:nested sampling
costs$molecular <- list(distn = "unif",
                        params = 7*staff_per_hr + c(min = 27, max = 35))

costs$visit1 <- list(distn = "gamma",
                     params = c(shape = 53.3, scale = 4.52))

costs$visit2 <- list(distn = "gamma",
                     params = c(shape = 18.78, scale = 7.62))


#########
# times #
#########

time_res <- list(transcriptomic =
                   list(distn = "unif",
                        params = c(min = 1, max = 1.5)), #24-36 hours
                 proteomic_SELDI =
                   list(distn = "unif",
                        params = c(min = 0.5, max = 2)),
                 flow_cytometry =
                   list(distn = "unif",
                        params = c(min = 2, max = 7)),
                 molecular =
                   list(distn = "unif",
                        params = c(min = 2, max = 5)),
                 IGRA =
                   list(distn = "unif",
                        params = c(min = 2, max = 7)))


####################
# test performance #
####################

performance <- list(transcriptomic_rulein =
                      list(sens =
                             list(distn = "unif", #0.902
                                  params = c(min = 0.865, max = 0.932)),
                           spec =
                             list(distn = "unif", #0.552
                                  params = c(min = 0.493, max = 0.609))),
                    transcriptomic_ruleout =
                      list(sens =
                             list(distn = "unif", #0.294
                                  params = c(min = 0.245, max = 0.346)),
                           spec =
                             list(distn = "unif", #0.95
                                  params = c(min = 0.919, max = 0.972))),
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
                                  params = c(min = NA, max = NA))),
                    flow_cytometry_HIVneg =
                      list(sens =
                             list(distn = "unif", #66.7
                                  params = c(min = 0.548, max = 0.771)),
                           spec =
                             list(distn = "unif", #92.1
                                  params = c(min = 0.836, max = 0.971))),
                    molecular =
                      list(sens =
                             list(distn = "unif", #0.97
                                  params = c(min = 0.85, max = 1.0)),
                           spec =
                             list(distn = "unif", #0.2
                                  params = c(min = 0.057, max = 0.44))),
                    # QFT-GIT all patients from IDEA
                    ##TODO:use separate HIV negative estimates
                    ##TODO:beta distn
                    IGRA =
                      list(sens =
                             list(distn = "unif",
                                  params =  c(min = 0.673, max = 0.721)),
                           spec =
                             list(distn = "unif",
                                  params =  c(min = 0.761, max = 0.841))))


# save --------------------------------------------------------------------

save(costs, file = "data/costs.RData")
save(time_res, file = "data/time_res.RData")
save(performance, file = "data/performance.RData")

