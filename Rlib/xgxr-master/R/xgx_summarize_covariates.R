#'Summarize Covariate information in a dataset
#'
#' \code{xgx_summarize_covariates} 
#'
#'
#' @param data, the dataset to check. must contain a USUBJID or ID column for subject id
#' @param covariates, the column names of covariates, to explore
#' @param n_cts, the number of unique values for a covariate to be treated as continuous, default is 8
#'
#' @return list
#' @export
#' 
#' @import dplyr
#'
#' @examples
#' data = data.frame(ID=1:10,WT0 = rnorm(10,70,10),SEX=round(runif(10)))
#' x = xgx_summarize_covariates(data,c("WT0","SEX"))

xgx_summarize_covariates = function(data,covariates = NULL, n_cts = 8){
  #defining column names as variables, because this is a work around CRAN to accept the R package
  #due to the way dplyr and lazy evaluation interacts with the CRAN checking
  #https://stackoverflow.com/questions/48750221/dplyr-and-no-visible-binding-for-global-variable-note-in-package-check
  ID=NULL; USUBJID=NULL;
  
  if ("USUBJID" %in% names(data)) {
    data1 = filter(data,!duplicated(USUBJID))
  } else if ("ID" %in% names(data)) {
    data1 = filter(data,!duplicated(ID))
  } else {
    stop("data column USUBJID or ID is required")
  }

  icat=0
  icts=0
  catlist = list()
  ctslist = list()
  for (covk in covariates) {
    x = data1[[covk]]
    
    xdistinct = length(unique(x))
    xmissing  = sum(is.na(x))
    if (xdistinct>=n_cts) {
      
      icts=icts+1
      ctslist[[icts]] = tibble(Covariate     = covk,
                               Nmissing      = xmissing,
                               min           = min(x,na.rm=TRUE),
                               `25th`        = quantile(x,0.25,na.rm=TRUE),
                               median        = median(x,na.rm=TRUE),
                               `75th`        = quantile(x,0.75,na.rm=TRUE),
                               max           = max(x,na.rm=TRUE))
    } else {
      summ = tibble(var=x) %>%
        group_by(var) %>%
        count() %>%
        ungroup() %>%
        arrange(desc(n))
      
      icat=icat+1
      catlist[[icat]] = tibble(Covariate     = covk,
                               Nmissing      = xmissing,
                               Ndistinct     = xdistinct,
                               `Value (Count)` = paste0(summ$var," (",summ$n,")",collapse = ", "))
    }
  }
  
  #create summaries ----
  cat_table   = bind_rows(catlist)
  cts_table   = bind_rows(ctslist)
  
  output = list(cts_covariates  = cts_table,
                cat_covariates  = cat_table)
  
  #print the summary ----  
  # if (length(cts_table)>0) {
  #   cat("CONTINUOUS COVARIATES\n")
  #   pander::pander(cts_table)
  # } else {
  #   cat("NO CONTINUOUS COVARIATES\n")
  # }
  # 
  # if (length(cat_table)>0) {
  #   cat("CATEGORICAL COVARIATES\n")
  #   pander::panderOptions("table.split.cells",100)
  #   pander::pander(cat_table)
  # } else {
  #   cat("NO CATEGORICAL COVARIATES\n")
  # }
  return(output)
}

