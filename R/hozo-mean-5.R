#' Hozo (5)
#' 
#' Hozo et al.'s estimator for the sample mean from formula (5). Note that this
#' one does not depend on \eqn{n}.
#' 
#' @param a Sample minimum.
#' @param b Sample maximum.
#' @param m Sample median.
#' 
#' @export 

hozo_mean_5 <- function(a, b, m) {
  return(
    (a + 2 * m + b) / 4
  )
}
