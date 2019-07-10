#' Make a spaghetti plot layer
#' 
#' \code{xgx_geom_spaghetti} returns a ggplot layer spaghetti plot 
#'
#' @param mapping As in ggplot2; Default list of aesthetic mappings to use for plot. Must define x, y, and group for xgx_spaghetti.
#' @param data Default dataset to use for plot. If not already a data.frame, will be converted to one by fortify.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#' @param ... Other arguments passed on to methods. Not currently used. 
#' 
#' @export
#'
#' @examples
#' library(ggplot2)  
#' time = rep(seq(1,10),5)
#' id = sort(rep(seq(1,5), 10))
#' conc = exp(-time)*sort(rep(rlnorm(5),10))
#' 
#' data = data.frame(time, conc, id)
#' ggplot() + xgx_geom_spaghetti(data = data, mapping = aes(x = time, y = conc, group = id))


xgx_geom_spaghetti = function(...) {
  return(list(geom_point(...),
              geom_line(...)))
}

# xgx_geom_spaghetti = function(mapping = NULL, data = NULL, stat = "identity", position = "identity", 
#                               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
#   
#   missing_aes <- setdiff(c("x","y","group"), c(names(mapping)))
#   
#   if(length(missing_aes) > 0) stop(paste0("xgx_geom_spaghetti requires the following missing aesthetics: ", paste0(missing_aes, collapse = ", ")))
#   
#   if(is.null(mapping$color)){
#     mapping <- c(mapping, aes_string(color = paste0("factor(",mapping$group,")")))
#   }
#   class(mapping) <- "uneval"
#   
#   return(list(geom_point(mapping = mapping, data = data, stat = stat, position = position, 
#                          na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes, ...),
#               geom_line(mapping = mapping, data = data, stat = stat, position = position, 
#                         na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes, ...)))
#   
# }
