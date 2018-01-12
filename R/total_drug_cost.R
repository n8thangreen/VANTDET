
#' Calculate Total Drug Costs Per Day
#'
#' @param drug
#' @param weight
#'
#' @return
#' @export
#'
#' @examples
total_drug_cost <- function(drug,
                            weight = 1){

  with(drug, (cost_per_batch*dose_mg_day*weight)/(pill_per_batch*dose_per_pill))
}
