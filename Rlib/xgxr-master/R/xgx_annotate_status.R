#' Create a status (e.g. DRAFT) annotation layer
#' 
#' \code{xgx_annotate_status} adds a status (e.g. DRAFT) annotation layer to a plot. 
#' The text of the annotation can be customized, the default is "DRAFT". 
#' The color, location, size, fontface, transparency of the annotation can also be customized.
#'
#' @param status the text to 
#' @param x x location, default Inf (right most point)
#' @param y y location, default Inf (up most point)
#' @param color default "grey"
#' @param hjust horizontal justification, default 1.2
#' @param vjust vertical justification, default 1.2
#' @param fontsize font size to use, default 7
#' @param fontface font style to use, default "bold"
#' @param alpha transparency, default is 0.5
#' @param ... other arguments passed on to \code{\link{layer}}
#'
#' @return ggplot layer
#' @export
#'
#' @examples
#' library(ggplot2)  
#' data         = data.frame(x=1:1000,y=rnorm(1000))
#' ggplot(data=data,aes(x=x,y=y)) + 
#'   geom_point() +
#'   xgx_annotate_status("DRAFT")
#' 
xgx_annotate_status = function(status="DRAFT",
                               x=Inf, y=Inf, color="grey",
                               hjust=1.2, vjust=1.2,
                               fontsize=7, fontface="bold",
                               alpha = 0.5, ...) {
  annotate("text", x=x, y=y, 
           label=status, color=color, 
           hjust=hjust, vjust=vjust, 
           cex=fontsize, fontface=fontface,
           alpha = alpha, ...)
} 