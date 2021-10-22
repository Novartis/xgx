#' Wrapper for stat_smooth 
#' 
#' \code{xgx_stat_smooth} and \code{xgx_geom_smooth} produce smooth fits through continuous or categorical data. 
#' For categorical, ordinal, or multinomial data use method = polr. 
#' This wrapper also works with nonlinear methods like nls and nlsLM for continuous data. 
#' 
#' @seealso \code{\link{predictdf.nls}} for information on how nls confidence intervals are calculated.
#'
#'
#' @param mapping Set of aesthetic mappings created by `aes` or `aes_`. 
#' If specified and `inherit.aes = TRUE` (the default), it is combined with the 
#' default mapping at the top level of the plot. You must supply mapping if 
#' there is no plot mapping.
#' Warning: for `method = polr`, do not define `y` aesthetic, use `response` instead.
#' @param data The data to be displayed in this layer. There are three options:
#' 
#' If NULL, the default, the data is inherited from the plot data as specified 
#' in the call to ggplot.
#' 
#' A data.frame, or other object, will override the plot data. All objects 
#' will be fortified to produce a data frame. See fortify for which variables 
#' will be created.
#' 
#' A function will be called with a single argument, the plot data. The return 
#' value must be a data.frame., and will be used as the layer data.
#' @param level The percentile for the confidence interval (should fall 
#' between 0 and 1). The default is 0.95, which corresponds to a 95 percent 
#' confidence interval.
#' @param geom Use to override the default geom. Can be a list of multiple 
#' geoms, e.g. list("point","line","errorbar"), which is the default.
#' @param position Position adjustment, either as a string, or the result of 
#' a call to a position adjustment function.
#' 
#' @param method method (function) to use, eg. lm, glm, gam, loess, rlm. 
#' Example: `"polr"` for ordinal data. `"nlsLM"` for nonlinear least squares.
#' If method is left as `NULL`, then a typical `StatSmooth` is applied, 
#' with the corresponding defaults, i.e. For datasets with n < 1000 default is loess. 
#' For datasets with 1000 or more observations defaults to gam.
#' @param formula formula to use in smoothing function, eg. y ~ x, y ~ poly(x, 2), y ~ log(x)
#' @param se display confidence interval around smooth? (TRUE by default, see level to control)
#' @param fullrange should the fit span the full range of the plot, or just the data
#' @param n number of points to evaluate smoother at
#' @param span Controls the amount of smoothing for the default loess smoother. 
#' Smaller numbers produce wigglier lines, larger numbers produce smoother lines.
#' @param n_boot number of bootstraps to perform to compute confidence interval, 
#' currently only used for method = "polr", default is 200
#' @param method.args Optional additional arguments passed on to the method.
#' @param na.rm If FALSE, the default, missing values are removed with a 
#' warning. If TRUE, missing values are silently removed.
#' @param orientation The orientation of the layer, passed on to ggplot2::stat_summary. 
#' Only implemented for ggplot2 v.3.3.0 and later. The default ("x") summarizes y values over
#' x values (same behavior as ggplot2 v.3.2.1 or earlier). Setting \code{orientation = "y"} will 
#' summarize x values over y values, which may be useful in some situations where you want to flip
#' the axes, e.g. to create forest plots. Setting \code{orientation = NA} will try to automatically
#' determine the orientation from the aesthetic mapping (this is more stable for ggplot2 v.3.3.2
#' compared to v.3.3.0).
#' @param show.legend logical. Should this layer be included in the legends? 
#' NA, the default, includes if any aesthetics are mapped. FALSE never 
#' includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather 
#' than combining with them. This is most useful for helper functions that 
#' define both data and aesthetics and shouldn't inherit behaviour from the 
#' default plot specification, e.g. borders.
#' @param ... other arguments passed on to layer. These are often aesthetics, 
#' used to set an aesthetic to a fixed value, like color = "red" or size = 3. 
#' They may also be parameters to the paired geom/stat.
#'
#' @return ggplot2 plot layer
#'
#' @section Warning:
#' \code{nlsLM} uses \code{nls.lm} which implements the Levenberg-Marquardt
#' algorithm for fitting a nonlinear model, and may fail to converge for a
#' number of reasons. See \code{?nls.lm} for more information.
#' 
#' \code{nls} uses Gauss-Newton method for estimating parameters, 
#' and could fail if the parameters are not identifiable. If this happens 
#' you will see the following warning message: 
#' Warning message:
#' Computation failed in `stat_smooth()`:
#'   singular gradient
#'   
#' \code{nls} will also fail if used on artificial "zero-residual" data, 
#' use \code{nlsLM} instead.
#'
#' @examples 
#' 
#' # Example with nonlinear least squares (method = "nlsLM")
#' Nsubj <- 10
#' Doses <- c(0, 25, 50, 100, 200)
#' Ntot <- Nsubj*length(Doses)
#' times <- c(0,14,30,60,90)
#' 
#' dat1 <- data.frame(ID = 1:(Ntot),
#'                    DOSE = rep(Doses, Nsubj),
#'                    PD0 = stats::rlnorm(Ntot, log(100), 1),
#'                    Kout = exp(stats::rnorm(Ntot,-2, 0.3)),
#'                    Imax = 1,
#'                    ED50 = 25) %>%
#'   dplyr::mutate(PDSS = PD0*(1 - Imax*DOSE/(DOSE + ED50))*exp(stats::rnorm(Ntot, 0.05, 0.3))) %>%
#'   merge(data.frame(ID = rep(1:(Ntot), each = length(times)), Time = times), by = "ID") %>%
#'   dplyr::mutate(PD = ((PD0 - PDSS)*(exp(-Kout*Time)) + PDSS), 
#'                 PCHG = (PD - PD0)/PD0)
#' 
#' gg <- ggplot2::ggplot(dat1 %>% subset(Time == 90), 
#'                       ggplot2::aes(x = DOSE, y = PCHG)) +
#'   ggplot2::geom_boxplot(ggplot2::aes(group = DOSE)) +
#'   xgx_theme() +
#'   xgx_scale_y_percentchangelog10() +
#'   ggplot2::ylab("Percent Change from Baseline") +
#'   ggplot2::xlab("Dose (mg)")
#' 
#' gg +
#'   xgx_stat_smooth(method = "nlsLM", formula = y ~ E0 + Emax*x/(ED50 + x),
#'                   method.args = list(
#'                     start = list(Emax = -0.50, ED50 = 25, E0 = 0),
#'                     lower = c(-Inf, 0, -Inf)
#'                   ),
#'                   se = TRUE)
#'               
#' gg + 
#'   xgx_geom_smooth_emax()  
#'   
#' \dontrun{  
#' # example with ordinal data (method = "polr")
#' set.seed(12345)
#' data = data.frame(x = 120*exp(stats::rnorm(100,0,1)),
#'                   response = sample(c("Mild","Moderate","Severe"), 100, replace = TRUE),
#'                   covariate = sample(c("Male","Female"), 100, replace = TRUE)) %>%
#'   dplyr::mutate(y = (50 + 20*x/(200 + x))*exp(stats::rnorm(100, 0, 0.3)))
#'   
#' # example coloring by the response categories
#' xgx_plot(data = data) +
#'   xgx_stat_smooth(mapping = ggplot2::aes(x = x, response = response,
#'                                          colour = response, fill = response),
#'                   method = "polr") +
#'   ggplot2::scale_y_continuous(labels = scales::percent_format())
#' 
#' 
#' # example faceting by the response categories, coloring by a different covariate 
#' xgx_plot(data = data) +
#' xgx_stat_smooth(mapping = ggplot2::aes(x = x, response = response, 
#'                                        colour = covariate, fill = covariate),
#'                 method = "polr", level = 0.80) + 
#'                 ggplot2::facet_wrap(~response) + 
#'                 ggplot2::scale_y_continuous(labels = scales::percent_format())
#' }
#' 
#' @importFrom stats nls
#' @importFrom ggplot2 StatSmooth
#' @export
xgx_stat_smooth <- function(mapping = NULL,
                            data = NULL,
                            geom = "smooth",
                            position = "identity",
                            ...,
                            method = NULL,
                            formula = NULL,
                            se = TRUE,
                            n = 80,
                            span = 0.75,
                            n_boot = 200,
                            fullrange = FALSE,
                            level = 0.95,
                            method.args = list(),
                            na.rm = FALSE,
                            orientation = "x",
                            show.legend = NA,
                            inherit.aes = TRUE) {
  
  lays <- list()
  
  # Assume OLS / LM / nls / nlsLM / glm etc. model
  ggproto_stat <- ggplot2::StatSmooth
  
  # Default parameters
  gg_params = list(method = method,
                   formula = formula,
                   se = se,
                   n = n,
                   n_boot = n_boot,
                   fullrange = fullrange,
                   level = level,
                   na.rm = na.rm,
                   method.args = method.args,
                   span = span,
                   ...)
  
  # Compare to ggplot2 version 3.3.0
  # If less than 3.3.0, then don't include orientation option
  ggplot2_geq_v3.3.0 <- utils::compareVersion(as.character(utils::packageVersion("ggplot2")), '3.3.0') >= 0
  
  if(ggplot2_geq_v3.3.0){
    gg_params$orientation = orientation
  }else{
    if(!(orientation %in% "x")){
      warning('orientation other than "x" not supported for ggplot2 versions less than 3.3.0, setting orientation to "x"')
    }
    gg_params$orientation = "x"
  }
  
  # Class Model
  if (is.null(method)){ }
  else{
    if (method %in% c("polr")) {
      ggproto_stat <- StatSmoothOrdinal
      
      if(!(gg_params$orientation %in% c("y")) & !is.null(mapping$y)){
        if(is.null(mapping$response) ){
          mapping$response <- mapping$y
          warning("response aesthetic is not defined for ordinal data, but y is, reassigning y to response")
        }else{
          warning("y aesthetic is not used for ordinal data when orientation = 'x'")    
        }
        mapping$y <- NULL
      }
      
      if(!(gg_params$orientation %in% c("x")) & !is.null(mapping$x)){
        if(is.null(mapping$response) ){
          mapping$response <- mapping$x
          warning("response aesthetic is not defined for ordinal data, but x is, reassigning x to response")
        }else{
          warning("x aesthetic is not used for ordinal data when orientation = 'y'")    
        }
        mapping$x <- NULL
      }
      
    }
  }
  
  for (igeom in geom) {
    lay = ggplot2::layer(
      stat = ggproto_stat,
      data = data,
      mapping = mapping,
      geom = igeom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = gg_params
    )
    
    lays[[paste0("geom_", igeom)]] <- lay  
  }
  
  return(lays)
}

##' @importFrom minpack.lm nlsLM
##' @export
minpack.lm::nlsLM


#' Wrapper for stat_smooth 
#' 
#' @rdname xgx_stat_smooth
#' 
#' @export
#' 
xgx_geom_smooth <- function(mapping = NULL,
                            data = NULL,
                            geom = "smooth",
                            position = "identity",
                            ...,
                            method = NULL,
                            formula = NULL,
                            se = TRUE,
                            n = 80,
                            span = 0.75,
                            fullrange = FALSE,
                            level = 0.95,
                            method.args = list(),
                            na.rm = FALSE,
                            orientation = "x",
                            show.legend = NA,
                            inherit.aes = TRUE) {
  
  return(list(xgx_stat_smooth(mapping = mapping, 
                              data = data, 
                              geom = geom,
                              position = position,
                              method = method,
                              formula = formula,
                              se = se,
                              n = n,
                              span = span,
                              fullrange = fullrange,
                              level = level,
                              method.args = method.args,
                              na.rm = na.rm,
                              orientation = orientation,
                              show.legend = show.legend,
                              inherit.aes = inherit.aes,
                              ...)))
}

#' Plot Emax fit to data
#' 
#' \code{xgx_geom_smooth_emax} uses minpack.lm::nlsLM, predictdf.nls, and stat_smooth to display Emax model fit to data
#' 
#' @rdname xgx_stat_smooth
#' 
#' @export
xgx_geom_smooth_emax <- function(mapping = NULL, data = NULL, geom = "smooth",
                                 position = "identity", ..., method = "nlsLM", formula, 
                                 se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                 level = 0.95, method.args = list(), na.rm = FALSE,
                                 orientation = "x", show.legend = NA, inherit.aes = TRUE){
  if(missing(formula)) {
    warning("Formula not specified.\nUsing default formula y ~ E0 + Emax*x/(ED50 + x), 
            initializing E0, Emax, and ED50 to 1, 
            and setting lower bound on ED50 to 0")
    formula = y ~ E0 + Emax*x/(ED50 + x)
    method.args$start = list(E0 = 1, Emax = 1, ED50 = 1)
    method.args$lower = c(-Inf, -Inf, 0)
  }
  
  xgx_stat_smooth(mapping = mapping, data = data, geom = geom, 
                       position = position, ..., method = method, formula = formula,
                       se = se, n = n, span = span, fullrange = fullrange, 
                       level = level, method.args = method.args, na.rm = na.rm,
                       orientation = "x", show.legend = show.legend, inherit.aes = inherit.aes)
}

#' Prediction data frame from ggplot2
#' Get predictions with standard errors into data frame
#'
#' @param model model object
#' @param xseq newdata
#' @param se Display confidence interval around smooth?
#' @param level Level of confidence interval to use
predictdf <- function(model, xseq, se, level) UseMethod("predictdf")


#' Prediction data frame for nls
#' 
#' Get predictions with standard errors into data frame for use with geom_smooth
#'
#' \code{ggplot2::geom_smooth} produces confidence intervals by silently calling functions 
#' of the form predictdf.method, where method is "loess", "lm", "glm" etc. 
#' depending on what method is specified in the call to \code{geom_smooth}. 
#' Currently \code{ggplot2} does not define a \code{predictdf.nls} function for method of type "nls", 
#' and thus confidence intervals cannot be automatically generated by \code{geom_smooth} 
#' for method = "nls". Here we define \code{predictdf.nls} for calculating the confidence 
#' intervals of an object of type nls. \code{geom_smooth} will silently call this function 
#' whenever method = "nls", and produce the appropriate confidence intervals.
#' 
#' \code{predictdf.nls} calculates CI for a model fit of class nls based on the "delta-method" 
#' http://sia.webpopix.org/nonlinearRegression.html#confidence-intervals-and-prediction-intervals)
#'
#' CI = [ f(x0, beta) + qt_(alpha/2, n - d) * se(f(x0, beta)),
#'       f(x0, beta) + qt_(1 - alpha/2, n - d) * se(f(x0, beta))]
#'
#' where:
#' beta = vector of parameter estimates
#' x = independent variable
#' se(f(x0, beta)) = sqrt( delta(f)(x0, beta) * Var(beta) * (delta(f)(x0, beta))' )
#' delta(f) is the gradient of f
#'
#' @param model nls object
#' @param xseq newdata
#' @param se Display confidence interval around smooth?
#' @param level Level of confidence interval to use
#'
#' @return dataframe with x and y values, if se is TRUE dataframe also includes ymin and ymax
#'
#' @importFrom Deriv Deriv
#' @importFrom stats nls
#' @exportS3Method ggplot2::predictdf
predictdf.nls <- function(model, xseq, se, level) {
  
  # function to calculate gradient wrt model parameters
  # value is the function value
  # grad is the gradient
  fun_grad <- function(form, x, pars, se){
    
    # extract the model parameters to the local environment
    list2env(pars %>% as.list(), envir = environment())
    
    ret <- list()
    ret$value <- eval(form[[3L]]) # this is the value of the formula
    
    if(se){
      ret$grad <- list()
      xvec <- x      
      for(i in 1:length(xvec)){
        x = xvec[i]
        ret$grad[[i]] <- eval(Deriv::Deriv(form, names(pars), cache.exp = FALSE)) %>% as.list()
      }
      
      if(is.null(names(ret$grad[[i]]))){
        names(ret$grad[[i]]) <- names(pars)
      }
      
      
      ret$grad <- dplyr::bind_rows(ret$grad) %>% as.matrix
    }
    
    return(ret)
  }
  

  fg <- fun_grad(form = model$m$formula(), x = xseq, pars = model$m$getPars(), se)
  
  f.new <- fg$value # value of function
  pred <- data.frame(x = xseq, y = f.new)
  
  if(se){
    grad.new <- fg$grad # value of gradient
    
    
    vcov <- vcov(model)
    GS = rowSums((grad.new%*%vcov)*grad.new)
    
    alpha = 1 - level
    deltaf <- sqrt(GS)*qt(1 - alpha/2, df = summary(model)$df[2])
    
    pred$ymin <- f.new - deltaf
    pred$ymax <- f.new + deltaf
    
  }else{
    pred <- data.frame(x = xseq, y = f.new)
  }
  
  return(pred)
}


#' Prediction data frame for polr
#' 
#' Get predictions with standard errors into data frame for use with geom_smooth
#'
#' \code{predictdf.polr} is used by xgx_geom_smooth when method = "polr" 
#' to calculate confidence intervals via bootstraps.
#'
#' @param model object returned from polr
#' @param xseq sequence of x values for which to compute the smooth
#' @param se if TRUE then confidence intervals are returned
#' @param level confidence level for confidence intervals
#' @param data data to fit
#' @param method only works for method MASS::polr
#' @param formula formula to fit
#' @param method.args arguments to pass to method
#' @param weight weights to use for method
#' @param n_boot number of bootstraps to perform for confidence interval calculation, default is 200
#' 
#' @importFrom stats predict
#' 
#' @exportS3Method ggplot2::predictdf
predictdf.polr <- function(model, xseq, se, level, 
                           data, method, formula, method.args, weight, n_boot = 200){
  x <- y <- response <- NULL
  
  percentile_value <- level + (1 - level) / 2
  
  pred.df_boot = list()
  iter_failed = 0
  for (iboot in 1:n_boot) {
    new_pred <- tryCatch ({
      # Boostrap by resampling entire dataset
      #   (prediction + residual doesn't work with ordinal data)
      data_boot <- dplyr::sample_n(tbl = data,
                                   size = nrow(data),
                                   replace = TRUE)
      
      base.args <- list(quote(formula), data = quote(data_boot), weights = quote(weight))
      model_boot <- do.call(method, c(base.args, method.args))
      
      # Extract Bootstrapped Predictions
      # predictdf.polr(model_boot, xseq, se, level)
      
      pred <- stats::predict(model_boot, newdata = data.frame(x = xseq), type = "probs") %>%
        data.frame() %>%
        dplyr::mutate( x = xseq)
      pred.df <- tidyr::pivot_longer(data = pred, cols = -x, names_to = "response", values_to = "y")
      
    }, warning = function(w) {
      "There was a problem in the sampling."
    }
    )
    
    if (is.character(new_pred)) {
      iter_failed <- 1 + iter_failed
      next
    }
    
    pred.df_boot[[iboot]] <- new_pred
    
  }
  pred.df_boot <- dplyr::bind_rows(pred.df_boot) %>%
    dplyr::group_by(x, response) %>%
    dplyr::summarize(ymin = quantile(stats::na.omit(y), 1 - percentile_value),
                     ymax = quantile(stats::na.omit(y), percentile_value), .groups = "keep") %>%
    dplyr::ungroup()
  
  pred <- stats::predict(model, newdata = data.frame(x = xseq), type = "probs") %>%
    data.frame() %>%
    dplyr::mutate( x = xseq)
  
  pred.df <- tidyr::pivot_longer(data = pred, cols = -x, names_to = "response", values_to = "y")
  
  pred.df_group <- merge(pred.df, pred.df_boot, by = c("x","response"))
  
  ret <- pred.df_group %>% subset(,c("x", "y", "ymin", "ymax", "response"))
  
}


##' @importFrom gtable gtable
##' @export
gtable::gtable

#' Stat object for producing smooths through ordinal data
#' 
#' 
#' @importFrom ggplot2 ggproto
#' @export
StatSmoothOrdinal <- ggplot2::ggproto(
  "StatSmoothOrdinal", 
  ggplot2::Stat,
  
  required_aes = c("x", "response"),
  
  extra_params = c("na.rm", "orientation", "method","formula","se","n","span","fullrange","level","method.args","na.rm","n_boot","xseq"),
  
  compute_group = function(data, params) {
    return(data)
  },
  
  setup_params = function(self, data, params, ...) {
    
    # params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params$flipped_aes <- has_flipped_aes(data, params)
    
    required_aes <- self$required_aes
    
    if(params$flipped_aes){
      required_aes <- switch_orientation(self$required_aes)
    }
    
    msg <- character()
    
    if (is.null(params$formula)) {
      params$formula <- response ~ x
      msg <- c(msg, paste0("formula '", deparse(params$formula), "'"))
    }
    
    if (length(msg) > 0) {
      message("`geom_smooth()` using ", paste0(msg, collapse = " and "))
    }
    
    # check required aesthetics
    ggplot2:::check_required_aesthetics(
      required_aes,
      c(names(data), names(params)),
      ggplot2:::snake_class(self)
    )
    
    # Make sure required_aes consists of the used set of aesthetics in case of
    # "|" notation in self$required_aes
    required_aes <- intersect(
      names(data),
      unlist(strsplit(required_aes, "|", fixed = TRUE))
    )
    
    # aes_to_group are the aesthetics that are different from response,
    # it's assumed that these should split the data into groups for calculating CI,
    # e.g. coloring by a covariate
    #
    # aes_not_to_group are aesthetics that are identical to response,
    # it's assumed that these are only for applyng aesthetics to the end result, 
    # e.g. coloring by response category
    params$aes_to_group <- c()
    params$aes_not_to_group <- c()
    
    # go through PANEL, colour, fill, linetype, shape
    if( (data %>% subset(, c(response, PANEL)) %>% unique() %>% dim)[1] == length(unique(data$response) )){
      params$aes_not_to_group <- c(params$aes_not_to_group, "PANEL")
    }else{
      params$aes_to_group <- c(params$aes_to_group, "PANEL")
    }
    
    if(is.null(data$colour)){
      
    }else if((data %>% subset(, c(response, colour)) %>% unique() %>% dim)[1] == length(unique(data$response))){
      params$aes_not_to_group <- c(params$aes_not_to_group, "colour")
    }else{
      params$aes_to_group <- c(params$aes_to_group, "colour")
    }
    
    if(is.null(data$linetype)){
      
    }else if((data %>% subset(, c(response, linetype)) %>% unique() %>% dim)[1] == length(unique(data$response))){ 
      params$aes_not_to_group <- c(params$aes_not_to_group, "linetype")
    }else{
      params$aes_to_group <- c(params$aes_to_group, "linetype")
    }
    
    if(is.null(data$fill)){
      
    }else if((data %>% subset(, c(response, fill)) %>% unique() %>% dim)[1] == length(unique(data$response))){ 
      params$aes_not_to_group <- c(params$aes_not_to_group, "fill")
    }else{
      params$aes_to_group <- c(params$aes_to_group, "fill")
    }
    
    if(is.null(data$shape)){
      
    }else if((data %>% subset(, c(response, shape)) %>% unique() %>% dim)[1] == length(unique(data$response))){ 
      params$aes_not_to_group <- c(params$aes_not_to_group, "shape")
    }else{
      params$aes_to_group <- c(params$aes_to_group, "shape")
    }
    
    if(length(params$aes_not_to_group) == 0){
      warning("In xgx_stat_smooth: \n  No aesthetics defined to differentiate response groups.\n  Suggest to add color = response, linetype = response, or similar to aes() mapping.",
              call. = FALSE)
    }else{
      message(paste0("In xgx_stat_smooth: \n  The following aesthetics are identical to response: ", 
                     paste0(params$aes_not_to_group, collapse = ", "), 
                     "\n  These will be used for differentiating response groups in the resulting plot."))         
    }
    
    if(length(params$aes_to_group) > 0){
      message(paste0("In xgx_stat_smooth: \n  The following aesthetics are different from response: ", 
                     paste0(params$aes_to_group, collapse = ", "), 
                     "\n  These will be used to divide the data into different groups before calculating summary statistics on the response."))
    }
    
    params
  },
  
  setup_data = function(self, data, params, scales, xseq = NULL, method.args = list(), n_boot = 200) {
    
    data <- flip_data(data, params$flipped_aes)
    
    list2env(params, envir = environment())
    
    percentile_value <- level + (1 - level) / 2
    
    if(!is.factor(data$response)){
      data$response <- factor(data$response)
      message(paste0("In xgx_stat_smooth: \n  response should be a factor, converting to factor using as.factor(response) with default levels"))
    }
    
    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(new_data_frame())
    }
    
    if (is.null(data$weight)) data$weight <- 1
    
    if (is.null(xseq)) {
      if (is.integer(data$x)) {
        if (fullrange) {
          xseq <- scales$x$dimension()
        } else {
          xseq <- sort(unique(data$x))
        }
      } else {
        if (fullrange) {
          range <- scales$x$dimension()
        } else {
          range <- range(data$x, na.rm = TRUE)
        }
        xseq <- seq(range[1], range[2], length.out = n)
      }
    }
    
    if (is.character(method)) {
      if (identical(method, "polr")) {
        method <- MASS::polr
      } else {
        method <- match.fun(method)
      }
    }
    
    # base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
    
    # Define new grouping variable for which to split the data computation 
    # (excludes aesthetics that are identical to the Response variable)
    if(is.null(params$aes_to_group)){
      data <- data %>% dplyr::mutate(group2 = 1)
    }else{
      groups <- unique(data %>% subset(, params$aes_to_group))
      groups <- groups %>%
        dplyr::mutate(group2 = 1:dim(groups)[1])
      
      data <- data %>% merge(groups)
    }
    
    n_boot = n_boot
    prediction <- list()
    for(igroup in unique(data$group2)){
      idata <- data %>% subset(group2 == igroup)
      
      idata <- idata %>%
        mutate(response_orig = response) %>%
        mutate(response = paste0("X", as.numeric(response)) %>%
                 factor())
      
      base.args <- list(quote(formula), data = quote(idata), weights = quote(weight))
      
      model <- do.call(method, c(base.args, method.args))
      
      iprediction <- predictdf.polr(model, xseq, se, level, 
                                    data = idata, method, formula, method.args, weight, n_boot)
      
      iprediction <- merge(iprediction, idata %>% subset(,-c(x)), by = "response")
      
      iprediction <- iprediction %>%
        mutate(response = response_orig,
               response_orig = NULL)
      
      prediction[[igroup]] <- iprediction
    }
    
    prediction <- dplyr::bind_rows(prediction)
    
    prediction <- flip_data(prediction, params$flipped_aes)
    
    return(prediction)
    
  },

  compute_layer = function(self, data, params, layout) {
    data
  },
  
  compute_panel = function(data, params) {
    data
  }
  
)
