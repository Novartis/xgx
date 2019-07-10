#' Calls the standard theme for xGx graphics
#'
#' @export

xgx_theme <- function() {
  minor.color = "grey83"
  major.color = "grey83"
  
  theme_bw() + 
    theme(panel.grid.minor.x=element_line(color=minor.color),
          panel.grid.minor.y=element_line(color=minor.color),
          panel.grid.major.x=element_line(color=major.color),
          panel.grid.major.y=element_line(color=major.color))
}