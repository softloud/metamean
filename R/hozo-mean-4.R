#' Hozo (4)
#' 
#' Hozo et al.'s estimator for the sample mean from formula (4).
#' 
#' @param a Sample minimum.
#' @param b Sample maximum.
#' @param m Sample median.
#' @param n Sample size.
#' 
#' @export 

hozo_mean_4 <- function(a, b, m, n) {
  return(
    (a + 2 * m + b) / 4 + (a - 2 * m + b) / (4 * n)
  )
}
