#every time you update the functions in the package, this is how to reinstall
if (FALSE) #this is here so that you can source the file.  if you need to restart R, then run the next line
  .rs.restartR() #restarts R.  Try this if you are having problems with documentation

library(devtools)
library(stringr)
devtools::document() #update any documentation
#devtools::test() #run all the tests

#full install of the package
#withr::with_libpaths(new="./",devtools::install()) #install the package in the local directory
#                                                   #makes a temporary libpaths

#a quick way to load
devtools::load_all()

#load the vignettes
devtools::build_vignettes()
browseURL("inst/doc/xGx_Overview.html") #build and view the vignette (may be in this directory or the one below)
browseURL("doc/xGx_Overview.html") #build and view the vignette

#build the entire package for distribution
#this creates a tar.gz that can be used for install.package
if (FALSE)
  devtools::build()

#copy vignette source to inst for use in the xgx_pk_Rmd() function
file.copy("vignettes/PK_Multiple_Ascending_Dose.Rmd","inst/shell_Rmd/",overwrite = TRUE)

filename = "inst/shell_Rmd/PK_Multiple_Ascending_Dose.Rmd"
code = readLines(filename)

ind  = which(str_detect(code,"^author:"))
code[ind]= 'author:""'
code = str_replace_all(code,"tempdir()","getwd()")
writeLines(code,filename)


#load libraries packages uses
library(xgxr)
library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)

#detaching package if needed
#detach(package:xgxr)
