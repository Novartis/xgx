#' Create shell Rmarkdown for use in a project
#'
#' @param type, type of Rmarkdown to create, default is pk
#' @param to,   location to copy Rmarkdown file to, default is "."
#' @param open_file, logical flag.  If TRUE, then open the Rmarkdown in an editor
#'
#' @export
#' 
#' @importFrom utils file.edit head
#'
#' @examples
#' xgx_create_rmarkdown()

xgx_create_rmarkdown = function(type = "pk", to = ".", open_file = TRUE){
  Rmd = case_when(type == "pk" ~ "PK_Multiple_Ascending_Dose.Rmd",
                  TRUE         ~ "NA")
  if (Rmd=="NA") stop(paste0("type ",type, " not yet implemented"))
  filename = system.file("shell_Rmd", Rmd, package = "xgx", mustWork = TRUE)
  file.copy(filename, to, overwrite = FALSE)
  file_created = file.path(to,Rmd)
  message(file_created, " has been created")
  if (open_file == TRUE) file.edit(file_created)
}

