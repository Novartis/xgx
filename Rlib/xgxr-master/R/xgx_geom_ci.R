#' Plot data with mean and confidence intervals
#'
#' @param percentile is the percentile for the confidence interval.  default is .95  should be between 0.5 and 1
#' @param ... other arguments passed on to layer. These are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3. They may also be parameters to the paired geom/stat.

#' @return ggplot2 plot layer
#' @export
#'
#' @examples
#' library(ggplot2)  
#' data = data.frame(x = rep(c(1,2,3),each=20),
#' y = rep(c(1,2,3),each=20) + rnorm(60))
#' ggplot(data,aes(x=x,y=y)) + 
#'  xgx_geom_ci(percentile = .95)

xgx_geom_ci = function(percentile=.95, ...) {
  
if (!(percentile > 0.5 && percentile < 1))
  stop("percentile should be greater than 0.5 and less than 1")

  percentile.value = percentile + (1-percentile)/2  

  return(list(xgx_stat_ci(percentile = percentile, ...)
))
}
