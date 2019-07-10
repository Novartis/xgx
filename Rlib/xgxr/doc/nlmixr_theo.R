## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----data_assignments, echo = TRUE, warning=FALSE, message=FALSE---------
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

# remove any variables that vignette might call from another vignette
rm(list=intersect(ls(),c("data","data_pk","data_pk_rich"))) 

# setting ggplot theme
xgx_theme_set()


## ---- echo = TRUE, warning=FALSE, message=FALSE--------------------------
#units of dataset
  time_units_dataset = "hours"
  time_units_plot    = "days"
  dose_label         = "Dose (mg)"
  conc_label         = "Concentration (ug/ml)"  
  concnorm_label     = "Normalized Concentration (ug/ml)/mg"
  
#covariates in the dataset
  covariates = c("WT")
  
#load dataset 
  data = nlmixr_theo_sd
  
#make sure that the necessary columns are assigned
#five columns are required: TIME, LIDV, CMT, DOSE, DOSEREG
  data = data %>%
    mutate(ID      = ID) %>%   #ID column
    group_by(ID) %>%
    mutate(TIME    = TIME,     #TIME column name 
           NOMTIME = as.numeric(cut(TIME,breaks = c(-Inf,.1,.7,1.5,4,8,10.5,15,Inf), 
                                         labels = c( 0,.5, 1,  2,7,9  ,12, 24))),
           EVID    = EVID,                  #EVENT ID >=1 is dose, 0 otherwise
           CYCLE   = 1,                     #CYCLE of PK data 
           LIDV    = DV,                    #DEPENDENT VARIABLE column name
           CENS    = 0,                     #CENSORING column name
           CMT     = CMT,                   #COMPARTMENT column here (e.g. CMT or YTYPE)
           DOSE    = signif(max(AMT)*WT,2), #DOSE column here (numeric value) - convert mg/kg (in dataset) to mg
           DOSEREG = DOSE) %>% #DOSE REGIMEN column here
    ungroup()
  
#convert DOSEREG to factor for proper ordering in the plotting
#add LIDVNORM dose normalized concentration for plotting
  data = data %>%
    arrange(DOSE) %>%
    mutate(LIDVNORM    = LIDV/DOSE,
           DOSEREG     = factor(DOSEREG, levels =     unique(DOSEREG)),
           DOSEREG_REV = factor(DOSEREG, levels = rev(unique(DOSEREG)))) #define order of treatment factor
  
#for plotting the PK data
  data_pk = filter(data,CMT==2)
  
#NCA
NCA = data %>%
  filter(CMT==2, NOMTIME>0, NOMTIME<=24) %>%
  group_by(ID) %>%
  summarize(AUC_0_24     = trapz(TIME,LIDV),
            Cmax_0_24    = max(LIDV),
            Ctrough_0_24 = LIDV[length(LIDV)],
            DOSE         = DOSE[1],
            WT           = WT[1]) %>%
  gather(PARAM,VALUE,-c(ID,DOSE,WT)) %>%
  mutate(VALUE_NORM = VALUE/DOSE) %>%
  ungroup()

# directories for saving
dirs = list(
  parent_dir        = tempdir(),
  rscript_dir       = "./",
  rscript_name      = "PK_Multiple_Ascending_Dose.Rmd",
  results_dir       = "./",
  filename_prefix   = "theo_PK_")

#flag for labeling figures as draft
  draft_flag      = "DRAFT"


## ------------------------------------------------------------------------
  check = xgx_check_data(data,covariates)

  kable(check$summary)
  kable(head(check$data_subset))
  kable(check$cts_covariates)
  kable(check$cat_covariates)


## ----  echo = TRUE, warning=FALSE, message=FALSE, fig.width = 8, fig.height = 5----
gg = ggplot(data = data_pk, aes(x = NOMTIME, y = LIDV, group=DOSE, color = DOSEREG_REV)) 
gg = gg + xgx_stat_ci()
gg = gg + xgx_scale_x_time_units(time_units_dataset, time_units_plot)
gg = gg + labs(y = conc_label, color = "Dose")
glin = xgx_save(width = 5, height = 5, dirs, "Summary_Lin", draft_flag)

gg = glin + xgx_scale_y_log10()
glog = xgx_save(width = 5, height = 5, dirs, "Summary_Log", draft_flag)

g2 = arrangeGrob(glin,glog,nrow = 1)
grid.arrange(g2)

## ----  echo = TRUE, warning=FALSE, message=FALSE, fig.width = 6, fig.height = 4----
if (exists("data_pk_rich")) {
  gg = ggplot(data_pk_rich, aes(x = PROFTIME, y = LIDV, group= interaction(CYCLE,DOSE), color = DOSEREG_REV)) 
  gg = gg + facet_grid(~DAY_label, scales = "free_x")
  gg = gg + xgx_stat_ci()
  gg = gg + xgx_scale_x_time_units(time_units_dataset, time_units_plot)
  gg = gg + xgx_scale_y_log10()
  gg = gg + labs(y = conc_label, color = "Dose")
  xgx_save(width = 6, height = 4, dirs, "Summary_Cycle", draft_flag)
}

## ----  echo = TRUE, warning=FALSE, message=FALSE, fig.width = 10, fig.height = 3----
gg = ggplot(data = data_pk, aes(x = NOMTIME, y = LIDV, group= interaction(ID,CYCLE))) 
gg = gg + geom_line(size = 1, color = rgb(0.5,0.5,0.5), alpha = 0.3) 
gg = gg + geom_point(aes(color=factor(CENS),shape=factor(CENS)),size = 2, alpha = 0.3) 
gg = gg + xgx_stat_ci(aes(group=NULL, color = NULL))
gg = gg + facet_grid(.~DOSEREG)
gg = gg + xgx_scale_x_time_units(time_units_dataset, time_units_plot)
gg = gg + xgx_scale_y_log10()
gg = gg + ylab(conc_label)
gg = gg + theme(legend.position="none") 
gg = gg + scale_shape_manual(values=c(1,8))
gg = gg + scale_color_manual(values=c("grey50","red"))
xgx_save(width = 6, height = 4, dirs, "Summary_Spaghetti", draft_flag)

## ----  echo = TRUE, warning=FALSE, message=FALSE, fig.width = 10, fig.height = 5----
gg = ggplot(data = data_pk, aes(x = TIME, y = LIDV, group = interaction(ID,CYCLE), color = factor(DOSEREG_REV), shape = factor(CENS)) )
gg = gg + geom_line(size = 1, alpha = 0.5) 
gg = gg + geom_point()
gg = gg + xgx_scale_x_time_units(time_units_dataset, time_units_plot)
gg = gg + xgx_scale_y_log10()
gg = gg + labs(y = conc_label, color = "Dose", shape = "Censoring")
xgx_save(width = 4, height = 4, dirs, "Variability", draft_flag)

## ----  echo = TRUE, warning=FALSE, message=FALSE, fig.width = 7, fig.height = 4----
if (exists("data_pk_rich")) {
  gg = ggplot(data = data_pk_rich, aes(x = TIME, y = LIDV, group = interaction(ID,CYCLE), color = DOSEREG_REV, shape = factor(CENS)))
  gg = gg + geom_line(size = 1, alpha = 0.5) 
  gg = gg + geom_point()
  gg = gg + facet_grid(~DAY_label,scales = "free_x")
  gg = gg + xgx_scale_x_time_units(time_units_dataset, time_units_plot)
  gg = gg + xgx_scale_y_log10()
  gg = gg + labs(y = conc_label, color = "Dose", shape = "Censoring")
  xgx_save(width=7,height=4,dirs,"Variability2",draft_flag)
}

## ----  echo = TRUE, warning=FALSE, message=FALSE, fig.width = 7, fig.height = 3----
gg = ggplot(data = data_pk, aes(x = TIME, y = LIDV,group = interaction(ID,CYCLE), color = factor(CENS), shape = factor(CENS)))
gg = gg + geom_line(size = 1, alpha = 0.5) 
gg = gg + geom_point()
gg = gg + facet_grid(.~DOSEREG)
gg = gg + xgx_scale_x_time_units(time_units_dataset, time_units_plot)
gg = gg + xgx_scale_y_log10()
gg = gg + ylab(conc_label)
gg = gg + scale_shape_manual(values=c(1,8))
gg = gg + scale_color_manual(values=c("grey50","red"))
gg = gg + theme(legend.position="none") 
xgx_save(width=7,height=4,dirs,"Variability3",draft_flag)

## ----  echo = TRUE, warning=FALSE, message=FALSE, fig.width = 4, fig.height = 4----
gg = ggplot(data = data_pk, aes(x = NOMTIME, y = LIDVNORM, group= DOSEREG_REV, color = DOSEREG_REV)) 
gg = gg + xgx_stat_ci()
gg = gg + xgx_scale_x_time_units(time_units_dataset, time_units_plot)
gg = gg + xgx_scale_y_log10()
gg = gg + labs(y = conc_label, color="Dose")
xgx_save(width=4,height=4,dirs,"DoseNorm",draft_flag)

## ----  echo = TRUE, warning=FALSE, message=FALSE, fig.width = 7, fig.height = 4----
if (exists("data_pk_rich")) {
  gg = ggplot(data_pk_rich, aes(x=NOMTIME,y=LIDVNORM,group = interaction(DOSE,CYCLE),color=DOSEREG_REV))
  gg = gg + xgx_stat_ci()
  gg = gg + facet_grid(~DAY_label,scales = "free_x")
  gg = gg + xgx_scale_x_time_units(time_units_dataset, time_units_plot)
  gg = gg + xgx_scale_y_log10()
  gg = gg + labs(y = conc_label, color="Dose")
  xgx_save(width=7,height=4,dirs,"DoseNorm2",draft_flag)
}

## ----  echo = TRUE, warning=FALSE, message=FALSE, fig.width = 8, fig.height = 8----
gg = ggplot(data = data_pk, aes(x = TIME, y = LIDV)) 
gg = gg + geom_line() 
gg = gg + geom_point(aes(color=factor(CENS),shape=factor(CENS))) 
gg = gg + facet_wrap(~ID+DOSEREG)
gg = gg + xgx_scale_x_time_units(time_units_dataset, time_units_plot)
gg = gg + xgx_scale_y_log10()
gg = gg + ylab(conc_label)
gg = gg + theme(legend.position="none") 
gg = gg + scale_shape_manual(values=c(1,8))
gg = gg + scale_color_manual(values=c("black","red"))
xgx_save(width = 8, height = 8, dirs, "Individual", draft_flag)

## ----  echo = TRUE, warning=FALSE, message=FALSE, fig.width = 8, fig.height = 4----
if (!exists("NCA")) {
  warning("For PK data exploration, it is highly recommended to perform an NCA")
} else {
  gg = ggplot(data = NCA, aes(x = DOSE, y = VALUE_NORM))
  gg = gg + geom_boxplot(aes(group = DOSE)) 
  gg = gg + geom_smooth(method = "loess", color = "black")
  gg = gg + facet_wrap(~PARAM, scales = "free_y") 
  gg = gg + xgx_scale_x_log10(breaks = unique(NCA$DOSE))
  gg = gg + xgx_scale_y_log10()
  gg = gg + labs(x = dose_label, y = concnorm_label)
  xgx_save(width = 4, height = 5, dirs, "NCA_Normalized", draft_flag)
}

## ----  echo = TRUE, warning=FALSE, message=FALSE, fig.width = 4, fig.height = 6----
if (!exists("NCA")) {
  warning("For covariate exploration, it is highly recommended to perform an NCA")
} else {
  NCA_cts = NCA[,c("PARAM","VALUE",check$cts_covariates$Covariate)] %>%
    gather(COV,COV_VALUE,-c(PARAM,VALUE))
  
  NCA_cat = NCA[,c("PARAM","VALUE",check$cat_covariates$Covariate)] %>%
    gather(COV,COV_VALUE,-c(PARAM,VALUE))
  
  if (nrow(check$cts_covariates)>=1) {
    gg = ggplot(data = NCA_cts, aes(x = COV_VALUE, y = VALUE))
    gg = gg + geom_point()
    gg = gg + geom_smooth(method = "loess", color = "black")
    gg = gg + facet_grid(PARAM~COV,switch = "y", scales = "free_y")
    gg = gg + xgx_scale_x_log10()
    gg = gg + xgx_scale_y_log10()
    gg = gg + labs(x = "Covariate Value", y = "NCA Parameter Value")
    gg = xgx_save(width = 6, height = 6, dirs, "NCA_CtsCov", draft_flag)
    print(gg)
  }
  
  if (nrow(check$cat_covariates)>=1) {
    gg = ggplot(data = NCA_cat, aes(x = COV_VALUE, y = VALUE))
    gg = gg + geom_boxplot()
    gg = gg + facet_grid(PARAM~COV,switch = "y", scales = "free_y")
    gg = gg + xgx_scale_y_log10()
    gg = gg + labs(x = "Covariate Value", y = "NCA Parameter Value")
    gg = xgx_save(width = 6, height = 6, dirs, "NCA_CatCov", draft_flag)
    print(gg)
  }
}

