#' Saving plot, automatically annotating the status and denoting the filenames
#'
#' @param width width of plot
#' @param height height of plot
#' @param dirs list of directories.  If NULL or if directories missing, there is default behavior below
#'
#' \enumerate{
#' \item parent_dir  = Parent directory containing the Rscript and the Results folder, default getwd()
#' \item rscript_dir = Subdirectory of parent_dir that contains the Rscript used to generate the figure, default "./"
#' \item rscript_name= Name of the Rscript used to generate the figure, default "Name_Of_Script_Here.R"
#' \item results_dir = Subdirectory ofparent_dir where the figure is stored, default "./"
#' \item filename_prefix = prefix of filename to be appended to filename_main
#' }
#' 
#' @param filename_main main part of the filename, excluding prefix and suffix.  no default
#' @param status status to be annotated
#' @param g ggplot plot object, default is ggplot::last_plot()
#' @param filetype file extension (e.g. "pdf","csv" etc.)
#' @param status_x x location of the status in plot
#' @param status_y y location of the status in plot
#' @param status_fontsize font size for status in plot
#'
#' @return ggplot2 plot object
#' @export
#'
#' @examples
#' 
#' library(ggplot2)  
#' dirs = list(
#'   parent_dir= "./",
#'   rscript_dir  = "./",
#'   rscript_name = "Example.R",
#'   results_dir  = "./",
#'   filename_prefix   = "Task01_")
#' data = data.frame(x=1:1000,y=rnorm(1000))
#' ggplot(data=data,aes(x=x,y=y)) +
#' geom_point()
#' xgx_save(4,4,dirs,"Example","DRAFT")

xgx_save = function(width,
                    height,
                    dirs              = NULL,
                    filename_main,
                    status,
                    g = last_plot(),
                    filetype="png",
                    status_x=Inf,
                    status_y=Inf,
                    status_fontsize=7) {
  
  if (is.null(dirs$parent_dir))      dirs$parent_dir      = getwd()
  if (is.null(dirs$rscript_dir))     dirs$rscript_dir     = "./"
  if (is.null(dirs$rscript_name))    dirs$rscript_name    = "Name_Of_Script_Here.R"
  if (is.null(dirs$results_dir))     dirs$results_dir     = "./"
  if (is.null(dirs$filename_prefix)) dirs$filename_prefix = ""
  
  filedir        = file.path(dirs$results_dir)
  dirs$filename  = paste0(dirs$filename_prefix,filename_main,".",filetype)         #get the full filename
  
  g = g + xgx_annotate_filenames(dirs)
  g = g + xgx_annotate_status(status,x=status_x,y=status_y,fontsize=status_fontsize)
  
  ggsave(plot=g,width=width,height=height,file.path(filedir,dirs$filename))
  return(g)
}
