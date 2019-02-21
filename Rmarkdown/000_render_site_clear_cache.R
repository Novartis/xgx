# This R script will compile the xGx website, 
# and move all necessary files within www folder to the parent directory
#

remove(list = ls())

system("rm -rf *_cache")

#########################################################
## render the website
#########################################################

# compiles all .Rmd files in this folder, 
# and once complete moves files to www folder
rmarkdown::render_site()

#########################################################
## The following lines will move the required website pages from
## the www folder into the root directory
#########################################################

# get contents of www folder
#www_contents = system("dir www/", intern = TRUE)
www_contents = list.files("www")

# extract list of *.html documents 
# and list of *_files directories (contains images for webpages)
files_html =  www_contents[grep(".html",www_contents)]
folders_files = www_contents[grep("_files",www_contents)]

# remove *.html documents from root directory
# and copy new *.html documents from www
system("rm ../*.html")
system(paste0("cp www/", files_html, " ../", files_html, collapse = "&"))

# remove *_files directories from root directory
# and copy new *_files from www
system("rm -r ../*_files")
system(paste0("cp -r www/", folders_files, " ../", folders_files, collapse = "&"))

# copy standard folders required for website from www to root
folders_to_replace = c("SiteResources","site_libs")
system(paste0("rm -r ../",folders_to_replace, collapse = "&"))
system(paste0("cp -r www/", folders_to_replace, " ../", folders_to_replace, collapse = "&"))

# remove www folder for clean up
system("rm -r www")
