#' @title
#'    Variance for pooled samples from two or more levels
#' @description
#'    This function calculates the "pooled" variance required for the t statistics.
#' @usage
#'    var.pooled(...)
#' @param ... Vectors of samples.
#' @export
var.pooled <- function(...){
  p <- list(...)
  df <- 0
  ss <- 0
  for(x in p){
    if(is.vector(x) & is.numeric(x)){
      n <- length(x)
      ss <- ss + (n-1)*var(x)
      df <- df + (n-1)
    }
  }
  v <- ss/df
  return(v)
}

#' @title
#'    Standard deviation for pooled samples from two or more levels
#' @description
#'    This function calculates the "pooled" standard deviation required for the t statistics.
#'    This function is a simple wrapper of 'var.pooled'.
#' @usage
#'    sd.pooled(...)
#' @param ... Vectors of samples.
#' @export
sd.pooled <- function(...){
  return(sqrt(var.pooled(...)))
}
