#' Saving table as an image, also labeling the program that created the table and where the table is stored
#'
#' @param data  data.frame or table of results
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
#' @param filename_main main part of the filename, excluding prefix and extension.  no default
#'
#' @importFrom utils write.csv
#'
#' @return ggplot2 plot object
#' @export
#'
#' @examples
#' data = data.frame(x=c(1,2),y=c(1,2))
#' xgx_save_table(data)


xgx_save_table = function(
                    data,
                    dirs = NULL,
                    filename_main) {
  
  if (is.null(dirs$parent_dir))      dirs$parent_dir      = getwd()
  if (is.null(dirs$rscript_dir))     dirs$rscript_dir     = "./"
  if (is.null(dirs$rscript_name))    dirs$rscript_name    = "Name_Of_Script_Here.R"
  if (is.null(dirs$results_dir))     dirs$results_dir     = "./"
  if (is.null(dirs$filename_prefix)) dirs$filename_prefix = ""
  
  filedir        = file.path(dirs$results_dir)
  dirs$filename  = paste0(dirs$filename_prefix,filename_main,".csv")         #get the full filename
  
  caption = with(dirs, c("",
                         parent_dir, 
                         paste0(rscript_dir, rscript_name),
                         paste0(results_dir, filename),
                         paste0("Created: ", Sys.time())))
  
  caption_row = data[1,] %>%
    mutate_all(function(x){x=""})
  caption_row = bind_rows(caption_row,caption_row,caption_row,caption_row,caption_row)
  caption_row[,1] = caption
  
  data_append = data %>%
    mutate_all(as.character) %>%
    bind_rows(caption_row)
  
  write.csv(data_append,paste0(filedir,dirs$filename),quote = FALSE, row.names = FALSE)
  return(data_append)
}
