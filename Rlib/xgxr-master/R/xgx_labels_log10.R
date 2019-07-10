#' Nice labels for log10.
#'
#' Returns a set of labels for ggplot
#'
#' @param breaks, breaks for the function
#'
#' @return either character or expression
#' @export
#'
#' @examples
#' print(xgx_labels_log10(c(1e-5,1,1e5)))

xgx_labels_log10 = function(breaks){
  labels = as.character(breaks) 
  if (all(log10(breaks)==as.integer(log10(breaks)),na.rm=TRUE))
     if (min(breaks,na.rm=TRUE)<.001 || max(breaks,na.rm=TRUE)>9999) #breaks can be NA sometime.  not sure why...
       #labels = parse(text = paste0("10^",log10(breaks)))
       labels = as.character(breaks)
  return(labels)
}


