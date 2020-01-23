# Overview

This repository displays suggested plots to pursue when exploring different PK/PD datasets, with a focus on exploring the Dose-Exposure-Response relationship. This site is a collection of exploratory plots and code, and could serve as a checklist of graphs someone might create for certain projects. 

Many of the codes on the site use functions that we have found to be helpful while exploring PK/PD data. We compiled these helpful functions into the xgxr R-package, which is available on [CRAN](https://cran.r-project.org/web/packages/xgxr/index.html), and [GitHub](https://github.com/Novartis/xgxr).

The contents of the xgx repository are as follows:
* The index.html file is the main page that links to all others.
* The majority of files and folders in the root directory are for displaying the website.
* The files most likely to be of interest to you are stored in these 3 subdirectories
    * Data       - contains the datasets
    * R          - contains R functions  
    * Rmarkdown  - contains Rmarkdown documents that make up the subpages
* If you update any of the files in the Rmarkdown folder, then to recompile the site, execute "Rmarkdown/000_render_site.R" in R

Most of the Rmarkdown scripts use caching so that recompiling is quick.  However, when developing the site, if you change any dependencies (e.g. functions in the R folder or data in the Data folder) you should delete the cache.  This means deleting all the folders in Rmarkdown that end in "_cache"  DO NOT DELETE SiteResources.

In order to add links to the website, do the following:
* Edit the Rmarkdown/_site.yml.  Under the menu of interest, add a text and href entry - follow other examples
* Edit the Rmarkdown/SiteResources/nav_icon.html.  Under the menu of interest, add the link and name.
* Then recompile with 000_render_site.R
