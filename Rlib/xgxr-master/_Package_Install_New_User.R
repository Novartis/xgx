## File contains various options for installing the xgxr package
## Try each option in order until one of them works for you

#############################################
## Option 1: Install directly from GitHub

# 1) Install package directly from GitHub - to run the first time you use the package
devtools::install_github("Novartis/xgxr", args = c('--library="./"'))

# 2) Load the package - to run every time you use the package
library(xgxr,lib.loc = "./")

#example code ----  
library(ggplot2)
ggplot(data=data.frame(x=rlnorm(1000,0,1),y=rlnorm(1000,0,3)),aes(x=x,y=y)) + 
  geom_point() + 
  xgx_theme() +
  xgx_scale_x_log10() + 
  xgx_scale_y_log10()

#############################################
## Option 2: Download package from GitHub and install
# 1) download the GitHub repository from the website below, copy it to a folder of interest and unzip
#   https://github.com/Novartis/xgxr
#
# 2) build the package - to run the first time you use the package
devtools::build("xgxr-master")

# 3) install the package - to run the first time you use the package
install.packages("xgxr-master",repos=NULL,lib="./",type="source")

# 4) load the library - to run every time you use the package
library(xgxr,lib.loc = "./")

#example code ----  
library(ggplot2)
ggplot(data=data.frame(x=rlnorm(1000,0,1),y=rlnorm(1000,0,3)),aes(x=x,y=y)) + 
  geom_point() + 
  xgx_theme() +
  xgx_scale_x_log10() + 
  xgx_scale_y_log10()


#############################################
## Option 3: Download package from GitHub and source functions manually

# 1) download the gitlab repository from the website below, copy it to a folder of interest and unzip
#   https://github.com/Novartis/xgxr

# 2) Source package functions manually
# Run this whenever you want to use the package's functions
Rfiles = list.files(path = "xgxr-master/R/", full.names = TRUE)
for(ifile in Rfiles){
  source(ifile)
}

# 3) Load in all the datasets manually
Rdafiles = list.files(path = "xgxr-master/data", full.names = TRUE)
for(ifile in Rdafiles){
  load(ifile)
}

#example code ----  
library(ggplot2)
ggplot(data=data.frame(x=rlnorm(1000,0,1),y=rlnorm(1000,0,3)),aes(x=x,y=y)) + 
  geom_point() + 
  xgx_theme() +
  xgx_scale_x_log10() + 
  xgx_scale_y_log10()
