#' Make PK summary plots
#' 
#' \code{xgx_PK_summary} returns a collection of PK summary plots. 
#' 
#' Plots include mean +/- CI plots in linear and log scale colored by treatment, 
#' and spaghetti plots faceted by treatment. The plots are returned arranged within a single grid.arrange object.
#'
#' @param data Default dataset to use for plot. If not already a data.frame, will be converted to one by fortify.
#' @param mapping As in ggplot2; Default list of aesthetic mappings to use for plot. Must define x, y, and group for xgx_spaghetti.
#' @param labels Labels to give to TIME, CONC, ID, and TRT (excluding units)
#' @param units_dataset Original units for TIME and CONC in the dataset
#' @param units_plot Modified units for TIME and CONC to include in the plot
#' @param conf_level Confidence level to use for making the confidence intervals in the summary plots.
#' @param ... Other arguments passed on to methods. Not currently used. 
#' @param environment If an variable defined in the aesthetic mapping is not found in the data, ggplot will look for it in this environment. It defaults to using the environment in which @ggplot() is called.
#'
#' @return a grid.arrange object
#' 
#' @import ggplot2
#' @importFrom grid unit.c
#' @export
#' 
#' @examples 
#' library(ggplot2)  
#' nsubj = 50
#' ntime = 8
#' time = rep(c(1,2,4,8,12,24,36,48),nsubj)
#' id = sort(rep(seq(1,nsubj), ntime))
#' trt = sort(rep(c(25,50,100,150,300), ntime*nsubj/5))
#' ka = rep(rlnorm(nsubj,-0.5,0.3),each =ntime)
#' ke = rep(rlnorm(nsubj,-3,0.3),each =ntime)
#' conc = trt*(ka*ke/(ka - ke))*(exp(-time*ke) - exp(-time*ka))*(rep(rlnorm(ntime*nsubj,0.3,0.1)))
#' 
#' data = data.frame(TIME = time, CONC = conc, ID = id, TRT = trt)
#' xgx_PK_summary(data = data, labels = list(TRT = "Dose"), 
#'   units_dataset = list(TIME = "Hours", CONC = "ng/mL", TRT = "mg"))

xgx_PK_summary <-  function(data = NULL, 
                            mapping = aes_string(TIME = "TIME", CONC = "CONC", ID = "ID", TRT = "TRT"), 
                            labels = list(TIME = "Time", CONC = "Concentration", ID = "ID", TRT = "Treatment"),
                            units_dataset = list(TIME = NULL, CONC = NULL, ID = NULL, TRT = NULL),
                            units_plot = list(TIME = NULL, CONC = NULL, ID = NULL, TRT = NULL),
                            conf_level=.95, ..., environment = parent.frame()){
  if(is.null(data)){
    stop("Please provide a dataset for plotting.")
  }
  if(is.null(mapping$TIME) | is.null(mapping$CONC) | is.null(mapping$ID) | is.null(mapping$TRT)){
    stop("Missing definitions for time, conc, subj_id, and trt_id in aes().")
  }
  if(is.null(labels$TIME)) labels$TIME = "Time"
  if(is.null(labels$CONC)) labels$CONC = "Concentration"
  if(is.null(labels$ID)) labels$ID = "ID"
  if(is.null(labels$TRT)) labels$TRT = "Treatment"
  
  if(is.null(units_plot$TIME)) units_plot$TIME = units_dataset$TIME

  mapping <- c(mapping, aes_string(x = paste0(mapping$TIME)))
  mapping <- c(mapping, aes_string(y = paste0(mapping$CONC)))
  ylabel <- paste0(labels$CONC, ifelse(is.null(units_dataset$CONC), "", paste0(" (", units_dataset$CONC, ")")))
  xlabel <- paste0(labels$TIME, ifelse(is.null(units_dataset$TIME), "", paste0(" (", units_plot$TIME, ")")))
  trtlabel <- paste0(labels$TRT, ifelse(is.null(units_dataset$TRT), "", paste0(" (", units_dataset$TRT, ")")))
  
  summary_mapping <- mapping
  summary_mapping <- c(summary_mapping, aes_string(color = paste0("factor(",summary_mapping$TRT,")")))
  summary_mapping <- c(summary_mapping, aes_string(group = paste0("factor(",summary_mapping$TRT,")")))
  class(summary_mapping) <- "uneval"
  
  spaghetti_mapping <- mapping
  spaghetti_mapping <- c(spaghetti_mapping, aes_string(color = NULL))
  spaghetti_mapping <- c(spaghetti_mapping, aes_string(group = paste0("factor(",spaghetti_mapping$ID,")")))
  spaghetti_mapping$TIME <- NULL
  spaghetti_mapping$CONC <- NULL
  spaghetti_mapping$ID <- NULL
  spaghetti_mapping$TRT <- NULL
  class(spaghetti_mapping) <- "uneval"
  
  gg_summary_linear <- xgx_plot(data = data, mapping = summary_mapping, ...) + 
    xgx_stat_ci(conf_level = conf_level, distribution = "normal", ...) + 
    guides(color = guide_legend(trtlabel)) +
    xgx_scale_x_time_units(units_dataset = units_dataset$TIME, units_plot = units_plot$TIME) + 
    ylab(ylabel) + xlab(xlabel) 
  
  gg_summary_log10 <- gg_summary_linear + 
    xgx_scale_y_log10()
  
  gg_spaghetti <- xgx_spaghetti(data = data, mapping = spaghetti_mapping, color = "black", alpha = 0.2, ...) + 
    facet_grid(paste0("~",mapping$TRT)) +  
                 xgx_scale_y_log10()  + 
    xgx_scale_x_time_units(units_dataset = units_dataset$TIME, units_plot = units_plot$TIME) +
    ylab(ylabel) + xlab(xlabel) +
                 theme(legend.position = "none") 
  
  grid_arrange_shared_legend <-
    function(...,
             ncol = length(list(...)),
             nrow = 1,
             position = c("bottom", "right")) {
      
      plots <- list(...)
      position <- match.arg(position)
      g <-
        ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
      legend <- g[[which(sapply(g, function(x)
        x$name) == "guide-box")]]
      lheight <- sum(legend$height)
      lwidth <- sum(legend$width)
      gl <- lapply(plots, function(x)
        x + theme(legend.position = "none"))
      gl <- c(gl, ncol = ncol, nrow = nrow)
      
      combined <- switch(
        position,
        "bottom" = arrangeGrob(
          do.call(arrangeGrob, gl),
          legend,
          ncol = 1,
          heights = grid::unit.c(unit(1, "npc") - lheight, lheight)
        ),
        "right" = arrangeGrob(
          do.call(arrangeGrob, gl),
          legend,
          ncol = 2,
          widths = unit.c(unit(1, "npc") - lwidth, lwidth)
        )
      )
      
      # grid::grid.newpage()
      # grid::grid.draw(combined)
      
      # return gtable invisibly
      invisible(combined)
      
    }
  
  
  gg <- grid.arrange(grid_arrange_shared_legend(gg_summary_linear, gg_summary_log10, nrow = 1), gg_spaghetti)
  
  ret <- gg 
  
  return(invisible(ret)); 
} 