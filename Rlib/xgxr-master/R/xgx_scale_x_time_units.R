#' Convert time units for plotting
#' 
#' \code{xgx_scale_x_time_units} converts x axis scale from one time unit to another.  
#' Supported units include hours, days, weeks, months, and years, which 
#' can also be called using just the first letter (h, d, w, m, y).
#'
#' @param units_dataset units of the input dataset, must be specified by user as "h", "d", "w", "m", or "y"
#' @param units_plot units of the plot, will be units of the dataset if empty
#' @inheritParams ggplot2::continuous_scale
#' @param ... other parameters for \code{ggplot2::scale_x_continuous}
#' 
#' xgx_scale_x_time_units
#'
#' @export
#' 
#' @examples 
#' data         = data.frame(x=1:1000,y=rnorm(1000))
#' ggplot(data=data,aes(x=x,y=y)) + 
#'   geom_point() + 
#'   xgx_scale_x_time_units(units_dataset="hours",units_plot="weeks")

#plotting a nice, scaled X axis
xgx_scale_x_time_units =
  function(units_dataset, units_plot=NULL, breaks = breaks_function, labels = labels_function, ...) {
    # h = hours, d=days, w=weeks, m=months, y=years
    
    if (is.null(units_plot)) units_plot = units_dataset
    
    #allows for user to write out longer string for units
    units_plot       = units_plot %>% tolower() %>% substr(1,1)
    units_dataset    = units_dataset  %>% tolower() %>% substr(1,1) #units_dataset)
    
    if (!(units_dataset %in% c("h","d","w","m","y")))
      stop("units_dataset must be hours, days, weeks, months, or years")
    if (!(units_plot %in% c("h","d","w","m","y")))
      stop("units_plot must be hours, days, weeks, months, or years")
    
    
    day.scale  = data.frame(h=1/24,
                            d=1,
                            w=7,
                            m=30.4375,
                            y=365.25)
    
    input.scale  = day.scale[[units_dataset]]
    output.scale = day.scale[[units_plot]]
    scale.factor = output.scale/input.scale
    
    breaks_function = function(data.range) 
      breaks = xgx_breaks_time(data.range/scale.factor,units_plot)*scale.factor
    
    labels_function = function(breaks)
      labels = breaks/scale.factor
    
    xlabel.list = data.frame(h="Hour",
                             d="Day",
                             w="Week",
                             m="Month",
                             y="Year")
    xlabel = xlabel.list[[units_plot]]
    
    return( list(
      scale_x_continuous(breaks=breaks_function, labels=labels_function, ...), 
      xlab(xlabel)) )
  }