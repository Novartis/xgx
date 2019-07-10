#' Make a spaghetti plot
#' 
#' \code{xgx_spaghetti} returns a spaghetti plot ggplot object
#'
#' @param data Default dataset to use for plot. If not already a data.frame, will be converted to one by fortify.
#' @param mapping As in ggplot2; Default list of aesthetic mappings to use for plot. Must define x, y, and group for xgx_spaghetti.
#' @param ... Other arguments passed on to methods. Not currently used. 
#' @param environment If an variable defined in the aesthetic mapping is not found in the data, 
#' ggplot will look for it in this environment. It defaults to using the environment in which \code{\link{ggplot}} is called.
#'
#' @export
#' 
#' @examples 
#' library(ggplot2)  
#' time = rep(seq(1,10),5)
#' id = sort(rep(seq(1,5), 10))
#' conc = exp(-time)*sort(rep(rlnorm(5),10))
#' 
#' data = data.frame(time = time, concentration  = conc, id = id)
#' xgx_spaghetti(data = data, mapping = aes(x = time, y = concentration, group = id))

xgx_spaghetti <-  function(data = NULL, mapping = aes(), ..., environment = parent.frame()){

  gg <- xgx_plot(data = data, mapping = mapping, ..., environment = environment) +
      xgx_geom_spaghetti(mapping = mapping, data = data, ...)
  
  ret <- gg
  
  return(ret);
}