
#' Inverse Sample
#'
#' @param cdf Cumulative density function
#'
#' @return sampled realisation
#' @export
#'
#' @examples
#'
inverse_sample <- function(cdf) {

  cdf$x[sum(runif(1) > cdf$y)]
}
