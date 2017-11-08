#' Hozo Equation 16
#'
#' Equation (16) from Hozo's paper. Estimates an approximation of the sample
#' standard deviation from some summary statistics.
#'
#' @param min Minimum of the sample.
#' @param max Maximum of the sample.
#' @param median Median of the sample.
#'
#' @export

hozo_16 <- function(min, median, max) {
  return(sqrt(1 / 12 * ((min - 2 * median + max) ^ 2 / 4 + (max - min) ^ 2)))
}
