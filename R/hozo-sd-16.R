#' Hozo (16)
#' 
#' Hozo et al.'s estimator for the sample standard deviation from formula (12). 
#' 
#' @param a Sample minimum.
#' @param b Sample maximum.
#' @param m Sample median.
#' 
#' @export 

hozo_sd_16 <- function(a, b, m) {
  return(
    sqrt(
      1 / 12 * (
        (a - 2 * m + b)^2 / 4 +
          (b - a)^2
      )
  )
  )
}
