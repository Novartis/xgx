#' log10 scales the x axis with a "pretty" set of breaks
#' 
#' \code{xgx_scale_x_log10} is similar to \code{ggplot2::scale_x_log10}.  
#' But it uses what we believe to be a nicer spacing and set of tick marks
#' it can be used the same as \code{scale_x_log10}
#'
#' @param breaks major breaks, default is a function defined here
#' @param minor_breaks minor breaks, default is a function defined here
#' @param labels function for setting the labels, defined here 
#' @param ... other arguments passed to \code{ggplot2::scale_x_log10}
#'
#' @export

xgx_scale_x_log10 <-  function(breaks       = xgx_breaks_log10, 
                               minor_breaks = xgx_log_breaks_minor,
                               labels       = xgx_labels_log10,
                               ...){
  xgx_log_breaks_minor <-  function(x){
    r1 <- range(log10(x));
    r <-  r1;
    r[1] <-  floor(r[1])
    r[2] <-  ceiling(r[2])+1;
    breaks <- c()
    for (i in seq(r[1],r[2])){
      breaks <-  c(breaks,seq(2*10^(i-1),10^i-10^(i-1),by=10^(i-1)));
    }
    breaks <-  breaks[breaks <= 10^r1[2]]
    breaks <-  breaks[breaks >= 10^r1[1]]
    return(breaks)
  }
  
  ret <- try(list(scale_x_log10(..., breaks=breaks, minor_breaks = minor_breaks, labels = labels)),
             silent=TRUE)
  if  (inherits(ret, "try-error")) return(scale_x_log10(...))
  return(ret);
}