#' Sets the default breaks for a time axis
#' 
#' \code{xgx_breaks_time} sets the default breaks for a time axis, 
#' given the units of the data and the units of the plot.
#' It is inspired by scales::extended_breaks
#'
#' for the extended breaks function, 
#' Q is a set of nice increments
#' w is a set of 4 weights for
#' \enumerate{
#' \item simplicity - how early in the Q order are you
#' \item coverage - labelings that don't extend outside the data: range(data)/range(labels)
#' \item density (previously granuality) - how cloes to the number of ticks do you get (default is 5)
#' \item legibility - has to do with fontsize and formatting to prevent label overlap
#' }
#' 
#' @references Talbot, Justin, Sharon Lin, and Pat Hanrahan. "An extension of Wilkinson’s 
#' algorithm for positioning tick labels on axes." IEEE Transactions on visualization and 
#' computer graphics 16.6 (2010): 1036-1043.
#' 
#'
#' @param data.range range of the data
#' @param units.plot units to use in the plot
#'
#' @export
#'
#' xgx_breaks_time
#'
#' @examples
#' library(ggplot2)  
#' xgx_breaks_time(c(0,5),"h")
#' xgx_breaks_time(c(0,6),"h")
#' xgx_breaks_time(c(-3,5),"h")
#' xgx_breaks_time(c(0,24),"h")
#' xgx_breaks_time(c(0,12),"h")
#' xgx_breaks_time(c(1,4),"d")
#' xgx_breaks_time(c(1,12),"d")
#' xgx_breaks_time(c(1,14),"d")
#' xgx_breaks_time(c(1,50),"d")
#' xgx_breaks_time(c(1000,3000),"d")
#' xgx_breaks_time(c(-21,100),"d")
#' xgx_breaks_time(c(-1,10),"w")

xgx_breaks_time <-  function(data.range,units.plot){
  dmin      = min(data.range)
  dmax      = max(data.range)
  dspan     = dmax - dmin
  m         = 5 #number of breaks to aim for
  Q.default = c(1, 5, 2, 4, 3,1) #default Q (spacing)
  w.default = c(0.25, 0.2, 0.5, 0.05)
  w.simple  = c(1,.2,.5,.05)
  
  
  if (units.plot %in% c("h","m") && dspan >= 48)       { 
    Q = c(24,12,6,3) 
    w = w.simple
  } else if (units.plot %in% c("h","m") && dspan >= 24)  {
    Q = c(3,12,6,2)
    w = w.simple
  } else if (units.plot %in% c("h","m") && dspan < 24) {
    Q = c(6,3,2,1)
    w = w.simple
  } else if (units.plot == "d" && dspan >= 12)  { 
    Q = c(7,14,28)
    w = w.simple
  } else {
    Q = Q.default
    w = w.default
  }

  breaks = labeling::extended(dmin,dmax,m,Q=Q,w=w)
  return(breaks)
}               



