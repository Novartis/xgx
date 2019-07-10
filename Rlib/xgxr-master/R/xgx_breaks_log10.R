#' Sets the default breaks for log10
#' 
#' \code{xgx_breaks_log10} sets nice breaks for log10 scale.
#' it's better than the default function because it ensures there is at least 2 breaks
#' and also, it will try to go by 3s (i.e. 1,3,10,30,100) if it makes sense
#'
#' for the extended breaks function Q is a set of nice increments and
#' w is a set of 4 weights for:
#' 
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
#'
#' @export
#'
#' @examples
#' library(ggplot2)  
#' xgx_breaks_log10(c(1,1000))
#' xgx_breaks_log10(c(.001,100))
#' xgx_breaks_log10(c(1e-4,1e4))
#' xgx_breaks_log10(c(1e-9,1e9))
#' xgx_breaks_log10(c(1,2))
#' xgx_breaks_log10(c(1,5))
#' xgx_breaks_log10(c(1,10))
#' xgx_breaks_log10(c(1,100))
#' xgx_breaks_log10(c(1,1.01))
#' xgx_breaks_log10(c(1,1.0001))
#' print(xgx_breaks_log10(c(1,1.000001)),digits=10)

xgx_breaks_log10 <-  function(data.range){
  dmin      = min(log10(data.range))
  dmax      = max(log10(data.range))
  m         = 5 #number of breaks to aim for
  Q         = c(1,0.5) #prefered breaks, in log10-space
  w.simple  = c(1,.2,.5,.05) #weights that heavily emphasize simpler breaks (factor of 10)
  
  breaks = labeling::extended(dmin,dmax,m,Q=Q)
  breaks = 10^breaks
  
  #ensure that there are at least 2 breaks
  #but also try to present "nice" breaks with only one significant digit
  breaks1 = unique(signif(breaks,1))
  breaks2 = unique(signif(breaks,2))
  breaks3 = unique(signif(breaks,3))
  if (length(breaks1)>=2) {
    breaks.out = breaks1
  } else if (length(breaks2)>=2) {
    breaks.out = breaks2
  } else if (length(breaks3)>=2) {
    breaks.out = breaks3
  } else {
    breaks.out = unique(breaks)
  }
  return(breaks.out)
}               



