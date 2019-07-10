#This script must be run with R/3.5.1
#It can be executed with the following command:
#
#source /CHBS/apps/busdev_apps/init.sh
#ml R/3.5.1-gomkl-2018a-X11-20171023
#R

if (as.numeric(version$minor) < 5) error("Requires R version 3.5 to work with ggplot2_3")
devtools::load_all() #simulates loading a package
devtools::build_vignettes()

