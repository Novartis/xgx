#get filename of current script being executed
#input should be sys.calls()
get_rscript_name = function(dirs          = list(), 
                            regexp_prefix = "^Task\\d\\d\\w?_") {
  ind = grep('.R',sys.calls())
  if (length(ind)==0) {  #function was likely called from R CMD BATCH, need a different way to get name  
    a = commandArgs()
    dirs$rscript_name = a[grep("\\.R$",a)]
  } else {
    ind = ind[length(ind)]
    dirs$rscript_name   = basename( as.character(sys.calls()[[ind]])[2] )
  }
  dirs$filename_prefix = str_extract(dirs$rscript_name,regexp_prefix)
  return(dirs)
}

#dirs = list()
x = get_rscript_name()
print(x)