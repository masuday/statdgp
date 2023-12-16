#' Histogram of samples with estimated density function of the normal distribution
#'
#' This function draws the histogram given samples with the normal probability density function on the same screen.
#' The PDF is estimated from the samples, i.e., 'mean(x)' and 'sd(x)'.
#'
#' @usage
#'     hist.norm(x, breaks="Sturges")
#' @param x A vector of samples.
#' @param breaks The parameter passed to 'hist'. The default is "Sturges".
#' @export
#' @examples
#'     x <- rnorm(1000,mean=50,sd=10)
#'     hist.norm(x)
#' @seealso https://stackoverflow.com/questions/2309123/how-to-plot-histogram-and-pdf-together-in-r
hist.norm <- function(x, breaks="Sturges"){
  h <- hist(x, plot=FALSE, breaks=breaks)
  plot(h, col="grey")
  m <- mean(x)
  s <- sd(x)
  xlines <-seq(min(h$breaks),max(h$breaks),length.out=100)
  lines(x = xlines,y=dnorm(xlines,mean=m,sd=s) *length(x)*diff(h$breaks)[1])
}

#' Histogram of samples with estimated density function of t-distribution
#'
#' This function draws the histogram given samples with the probability density function of the t distribution on the same screen.
#' The user gives the degree of freedom for t-distribution.
#'
#' @usage
#'     hist.t(x, df, breaks="Sturges")
#' @param x A vector of samples.
#' @param df Degree of freedom for 'dt', t-distribution.
#' @param breaks The parameter passed to 'hist'. The default is "Sturges".
#' @seealso https://stackoverflow.com/questions/2309123/how-to-plot-histogram-and-pdf-together-in-r
hist.t <- function(x, df, breaks="Sturges"){
  h <- hist(x, plot=FALSE, breaks=breaks)
  plot(h, col="grey")
  m <- mean(x)
  s <- sd(x)
  xlines <-seq(min(h$breaks),max(h$breaks),length.out=100)
  lines(x = xlines,y=dt(xlines,df=df) *length(x)*diff(h$breaks)[1])
}
