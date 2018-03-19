
is_TB <- function(data) {

  data$DosanjhGrouped %in% c("1", "2", "3")
}

is_notTB <- function(data) {

  data$DosanjhGrouped == "4"
}
