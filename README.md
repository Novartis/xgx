# Overview

This repository displays suggested plots to pursue when exploring different PK/PD datasets, with a focus on exploring the Dose-Exposure-Response relationship. This site is a collection of exploratory plots and code, and could serve as a checklist of graphs someone might create for certain projects.

* The index.html file is the main page that links to all others.
* The majority of files and folders in the root directory are for displaying the website.
* The files most likely to be of interest to you are stored in these 3 subdirectories
    * Data       - contains the datasets
    * R          - contains R functions  
    * Rmarkdown  - contains Rmarkdown documents that make up the subpages
* If you update any of the files in the Rmarkdown folder, then to recompile the site, execute "Rmarkdown/000_render_site.R" in R

Most of the Rmarkdown scripts use caching so that recompiling is quick.  However, when developing the site, if you change any dependencies (e.g. functions in the R folder or data in the Data folder) you should delete the cache.  This means deleting all the folders in Rmarkdown that end in "_cache"  DO NOT DELETE SiteResources.

# Data
* AE      - Fariba - blinded, to be explored how to share
* AUC     - Fariba - blinded, to be explored how to share
* dzz     - Andy   - blinded, can be shared
* mt12345 - Kostas - blinded, can be shared
* Multiple_Ascending_Dose_Dataset - Alison - consider deleting - check nothing uses it
* Multiple_Ascending_Dose_Dataset2 - Alison - the newer one to keep and document
* MultipleAscendingDose_PKPD - Andy - probably to delete - HAS BEEN DELETED
* Oncology - Fariba - blinded, to be explored how to share and which ones to keep and if they should be compressed
    * Only need Oncology_Efficacy_Data and Oncology_Efficacy_Dose
    * the rest to be deleted.
* PCtmp - Andy - keep
* PPtmp - Andy - keep
* Single_Ascending_Dose_Dataset - Alison - to document

# R
Andy to rename files and update

# Rmarkdown
