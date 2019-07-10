#' Saving table as an image, also labeling the program that created the table and where the table is stored
#'
#' @param data  data.frame or table of results
#' @param width width of table
#' @param height height of table
#' @param dirs list of directories.  If NULL or if directories missing, there is default behavior below
#' \enumerate{
#' \item parent_dir  = Parent directory containing the Rscript and the Results folder, default getwd()
#' \item rscript_dir = Subdirectory of parent_dir that contains the Rscript used to generate the figure, default "./"
#' \item rscript_name= Name of the Rscript used to generate the figure, default "Name_Of_Script_Here.R"
#' \item results_dir = Subdirectory ofparent_dir where the figure is stored, default "./"
#' \item filename_prefix = prefix of filename to be appended to filename_main
#' }
#' @param filename_main main part of the filename, excluding prefix and suffix.  no default
#' @param filetype file extension (e.g. "pdf","csv" etc.)
#'
#' @return ggplot2 plot object
#' @export
#'
#' @examples
#' data = data.frame(x=c(1,2),y=c(1,2))
#' xgx_save_table(data,4,4)


xgx_save_table_as_image = function(
                    data,
                    width,
                    height,
                    dirs              = NULL,
                    filename_main,
                    filetype="png",
                    parent_dir        = getwd(),
                    rscript_dir       = "./",
                    rscript_name      = "Name_Of_Script_Here.R",
                    results_dir       = "./",
                    filename_prefix   = "") {
  
  if (is.null(dirs$parent_dir))      dirs$parent_dir      = parent_dir
  if (is.null(dirs$rscript_dir))     dirs$rscript_dir     = rscript_dir
  if (is.null(dirs$rscript_name))    dirs$rscript_name    = rscript_name
  if (is.null(dirs$results_dir))     dirs$results_dir     = results_dir
  if (is.null(dirs$filename_prefix)) dirs$filename_prefix = rscript_dir
  
  filedir        = file.path(dirs$results_dir)
  dirs$filename  = paste0(dirs$filename_prefix,filename_main,".",filetype)         #get the full filename
  
  g = ggplot() 
  g = g + annotation_custom(gridExtra::tableGrob(data,
                                                 rows  = NULL,
                                                 theme = ttheme_minimal(core.just = "left")))
  g = g + xgx_annotate_filenames(dirs)

  ggsave(plot=g,width=width,height=height,file.path(filedir,dirs$filename))
  return(g)
}
