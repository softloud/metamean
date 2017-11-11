#' Hozo (12)
#' 
#' Hozo et al.'s estimator for the sample standard deviation from formula (12). 
#' 
#' @param a Sample minimum.
#' @param b Sample maximum.
#' @param m Sample median.
#' @param n Sample size.
#' 
#' @export 

hozo_sd_12 <- function(a, b, m, n) {
  return(
    sqrt(
      1 / (n - 1) * (
        a^2 + m^2 + b^2 +
          ((n - 3) / 2) *
          (((a + m)^2 + (m + b)^2)/4) - n * (
            (a + 2 * m + b) / 4 +
              (a - 2 * m + b) / (4 * n)
          )^2
      )
    )
  )
}
