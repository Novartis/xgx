## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = TRUE, warning=FALSE, message=FALSE--------------------------
#devtools::load_all()
library(xgxr)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(caTools)

#override masking
rename = dplyr::rename
select = dplyr::select
filter = dplyr::filter
count  = dplyr::count

# setting ggplot theme
xgx_theme_set()

## ----data_assignments, echo = TRUE, warning=FALSE, message=FALSE---------
#units of dataset
  time_units_dataset = "hours"
  time_units_plot    = "days"
  dose_label         = "Dose (mg)"
  conc_label         = "Concentration (ng/ml)"  
  concnorm_label     = "Normalized Concentration (ng/ml)/mg"
  
#covariates in the dataset
  covariates = c("WEIGHTB","SEX")
  
#load dataset 
  data     = Multiple_Ascending_Dose %>%
    filter(CMT %in% c(1,2))
  
#make sure that the necessary columns are assigned
#five columns are required: TIME, NOMTIME, LIDV, CMT, DOSE, DOSEREG
  data = data %>%
    mutate(TIME    = TIME,   #TIME column name 
           NOMTIME = NOMTIME,#NOMINAL TIME column name
           EVID    = EVID   ,#EVENT ID, >=1 is dose, otherwise measurement
           CYCLE   = CYCLE,  #CYCLE of PK data
           CYCLELAB= paste("Day", PROFDAY), #CYCLE LABEL for faceting
           LIDV    = LIDV,   #DEPENDENT VARIABLE column name
           CENS    = CENS,   #CENSORING column name
           CMT     = CMT,    #COMPARTMENT column here (e.g. CMT or YTYPE)
           DOSE    = DOSE,   #DOSE column here (numeric value)
           DOSEREG = TRTACT) #DOSE REGIMEN column here
  
#convert DOSEREG to factor for proper ordering in the plotting
#add LIDVNORM dose normalized concentration for plotting
  data = data %>%
    arrange(DOSE) %>%
    mutate(LIDVNORM    = LIDV/DOSE,
           DOSEREG     = factor(DOSEREG, levels =     unique(DOSEREG)),
           DOSEREG_REV = factor(DOSEREG, levels = rev(unique(DOSEREG)))) #define order of treatment factor
  
#for plotting the PK data
  data_pk = filter(data,CMT==2)

#for plotting the rich PK data and comparing cycles  
#this dataset is optional and this code can be deleted if the user
#does not have rich PK data that they want to compare.
  data_pk_rich = data_pk %>%
    filter(PROFDAY %in% c(1,6)) #PROFDAY is the day of the PK profile
  
#load NCA
  NCA      = Multiple_Ascending_Dose_NCA
  
#make sure NCA dataset has the appropriate columns
#five columns are required: DOSE, DOSEREG, PARAM, VALUE, VALUE_NORM
  NCA = NCA %>%
    mutate(DOSE       = DOSE,       #DOSE column here (numeric value)
           DOSEREG    = TRTACT,     #DOSE REGIMEN column here
           PARAM      = PARAM,      #PARAM name of the NCA parameter
           VALUE      = VALUE,       #VALUE of the NCA parameter
           VALUE_NORM = VALUE/DOSE) #DOSE NORMALIZED value of NCA parameter  
  
# directories for saving figures and tables
dirs = list(
  parent_dir        = tempdir(),
  rscript_dir       = "./",
  rscript_name      = "PK_Multiple_Ascending_Dose.Rmd",
  results_dir       = "./",
  filename_prefix   = "MAD_PK_")

#flag for labeling figures as draft
  draft_flag      = "DRAFT"

## ----check_data----------------------------------------------------------
  check = xgx_check_data(data,covariates)

  kable(check$summary)
  kable(head(check$data_subset))
  kable(check$cts_covariates)
  kable(check$cat_covariates)


