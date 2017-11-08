#' Hozo Equation 5
#'
#' Equation (5) from Hozo's paper. Estimates an approximation of the sample
#' mean from some summary statistics.
#'
#' @param min Minimum of the sample.
#' @param max Maximum of the sample.
#' @param median Median of the sample.
#'
#' @export

hozo_5 <- function(min, median, max) {
  return((min + 2 * median + max) / 4)
}
