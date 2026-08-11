## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7
)

## ----data_assignments, echo=TRUE, warning=FALSE, message=FALSE-----------
library(xgxr)
library(ggplot2)
library(dplyr)

# setting ggplot theme
xgx_theme_set()

## ---- echo=TRUE, warning=FALSE, message=FALSE----------------------------
# units of dataset
time_units_dataset <- "hours"
time_units_plot    <- "days"
dose_label         <- "Dose (mg)"
conc_label         <- "Concentration (ug/ml)"  
concnorm_label     <- "Normalized Concentration (ug/ml)/mg"
  
# covariates in the dataset
covariates <- c("WT")
  
# load dataset 
data <- nlmixr_theo_sd
  
# make sure that the necessary columns are assigned
# five columns are required: TIME, LIDV, CMT, DOSE, DOSEREG
data <- data %>%
  mutate(ID = ID) %>%   #ID column
  group_by(ID) %>%
  mutate(TIME = TIME,     #TIME column name 
         NOMTIME = as.numeric(cut(TIME,
                                  breaks = c(-Inf, 0.1, 0.7, 1.5, 4, 8, 10.5, 15, Inf), 
                                  labels = c( 0, 0.5, 1, 2, 7, 9, 12, 24))),
         EVID    = EVID,                     # EVENT ID >=1 is dose, 0 otherwise
         CYCLE   = 1,                        # CYCLE of PK data 
         LIDV    = DV,                       # DEPENDENT VARIABLE column name
         CENS    = 0,                        # CENSORING column name
         CMT     = CMT,                      # COMPARTMENT column here (e.g. CMT or YTYPE)
         DOSE    = signif(max(AMT) * WT, 2), # DOSE column here (numeric value)
                                             # convert mg/kg (in dataset) to mg
         DOSEREG = DOSE) %>% # DOSE REGIMEN column here
  ungroup()
  
# convert DOSEREG to factor for proper ordering in the plotting
# add LIDVNORM dose normalized concentration for plotting
data <- data %>%
  arrange(DOSE) %>%
  mutate(LIDVNORM    = LIDV / DOSE,
         DOSEREG     = factor(DOSEREG, levels = unique(DOSEREG)),
         DOSEREG_REV = factor(DOSEREG, levels = rev(unique(DOSEREG))))
                       # define order of treatment factor
  
# for plotting the PK data
data_pk <- filter(data, CMT == 2)
  
# NCA
NCA <- data %>%
  filter(CMT == 2, NOMTIME > 0, NOMTIME <= 24) %>%
  group_by(ID) %>%
  summarize(AUC_0_24     = caTools::trapz(TIME, LIDV),
            Cmax_0_24    = max(LIDV),
            Ctrough_0_24 = LIDV[length(LIDV)],
            DOSE         = DOSE[1],
            WT           = WT[1]) %>%
  tidyr::gather(PARAM, VALUE, -c(ID, DOSE, WT)) %>%
  mutate(VALUE_NORM = VALUE / DOSE) %>%
  ungroup()

## ------------------------------------------------------------------------
check <- xgx_check_data(data,covariates)

knitr::kable(check$summary)
knitr::kable(head(check$data_subset))
knitr::kable(check$cts_covariates)
knitr::kable(check$cat_covariates)

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
glin <- ggplot(data = data_pk, aes(x = NOMTIME,
                                   y = LIDV,
                                   group = DOSE,
                                   color = DOSEREG_REV)) +
  xgx_stat_ci() + 
  xgx_scale_x_time_units(time_units_dataset, time_units_plot) +
  labs(y = conc_label, color = "Dose")

glog <- glin + xgx_scale_y_log10()

gridExtra::grid.arrange(gridExtra::arrangeGrob(glin, glog, nrow = 1))

## ----  echo=TRUE, warning=FALSE, message=FALSE, fig.height=4-------------
if (exists("data_pk_rich")) {
  ggplot(data_pk_rich, aes(x = PROFTIME,
                           y = LIDV,
                           group = interaction(CYCLE,DOSE),
                           color = DOSEREG_REV)) +
    facet_grid(~DAY_label, scales = "free_x") + 
    xgx_stat_ci() +
    xgx_scale_x_time_units(time_units_dataset, time_units_plot) +
    xgx_scale_y_log10() +
    labs(y = conc_label, color = "Dose")
}

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = data_pk, aes(x = NOMTIME,
                           y = LIDV,
                           group = interaction(ID, CYCLE))) +
  geom_line(size = 1, color = rgb(0.5, 0.5, 0.5), alpha = 0.3) +
  geom_point(aes(color = factor(CENS), shape = factor(CENS)),
                     size = 2, alpha = 0.3) +
  xgx_stat_ci(aes(group = NULL, color = NULL)) +
  facet_grid(.~DOSEREG) +
  xgx_scale_x_time_units(time_units_dataset, time_units_plot) +
  xgx_scale_y_log10() +
  ylab(conc_label) +
  theme(legend.position = "none") +
  scale_shape_manual(values = c(1, 8)) +
  scale_color_manual(values = c("grey50", "red"))

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=5--------------
ggplot(data = data_pk, aes(x = TIME,
                           y = LIDV,
                           group = interaction(ID, CYCLE),
                           color = factor(DOSEREG_REV),
                           shape = factor(CENS))) + 
  geom_line(size = 1, alpha = 0.5) +
  geom_point() +
  xgx_scale_x_time_units(time_units_dataset, time_units_plot) +
  xgx_scale_y_log10() +
  labs(y = conc_label, color = "Dose", shape = "Censoring")

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=4--------------
if (exists("data_pk_rich")) {
  ggplot(data = data_pk_rich, aes(x = TIME,
                                  y = LIDV,
                                  group = interaction(ID, CYCLE),
                                  color = DOSEREG_REV,
                                  shape = factor(CENS))) +
    geom_line(size = 1, alpha = 0.5) +
    geom_point() +
    facet_grid(~DAY_label, scales = "free_x") +
    xgx_scale_x_time_units(time_units_dataset, time_units_plot) +
    xgx_scale_y_log10() +
    labs(y = conc_label, color = "Dose", shape = "Censoring")
}

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = data_pk, aes(x = TIME,
                           y = LIDV,
                           group = interaction(ID, CYCLE),
                           color = factor(CENS),
                           shape = factor(CENS))) + 
  geom_line(size = 1, alpha = 0.5) + 
  geom_point() + 
  facet_grid(.~DOSEREG) + 
  xgx_scale_x_time_units(time_units_dataset, time_units_plot) + 
  xgx_scale_y_log10() + 
  ylab(conc_label) + 
  scale_shape_manual(values = c(1, 8)) + 
  scale_color_manual(values = c("grey50", "red")) + 
  theme(legend.position = "none")

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = data_pk, aes(x = NOMTIME,
                           y = LIDVNORM,
                           group = DOSEREG_REV,
                           color = DOSEREG_REV)) +
  xgx_stat_ci() +
  xgx_scale_x_time_units(time_units_dataset, time_units_plot) +
  xgx_scale_y_log10() +
  labs(y = conc_label, color = "Dose")

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=4--------------
if (exists("data_pk_rich")) {
  ggplot(data_pk_rich, aes(x = NOMTIME,
                           y = LIDVNORM,
                           group = interaction(DOSE, CYCLE),
                           color = DOSEREG_REV)) +
    xgx_stat_ci() +
    facet_grid(~DAY_label,scales = "free_x") +
    xgx_scale_x_time_units(time_units_dataset, time_units_plot) +
    xgx_scale_y_log10() +
    labs(y = conc_label, color = "Dose")
}

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=7--------------
ggplot(data = data_pk, aes(x = TIME, y = LIDV)) +
  geom_line() +
  geom_point(aes(color = factor(CENS), shape = factor(CENS))) + 
  facet_wrap(~ID + DOSEREG) +
  xgx_scale_x_time_units(time_units_dataset, time_units_plot) +
  xgx_scale_y_log10() +
  ylab(conc_label) +
  theme(legend.position = "none") +
  scale_shape_manual(values = c(1, 8)) +
  scale_color_manual(values = c("black", "red"))

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=4--------------
if (!exists("NCA")) {
  warning("For PK data exploration, it is highly recommended to perform an NCA")
} else {
  ggplot(data = NCA, aes(x = DOSE, y = VALUE_NORM)) +
    geom_boxplot(aes(group = DOSE)) +
    geom_smooth(method = "loess", color = "black") +
    facet_wrap(~PARAM, scales = "free_y") +
    xgx_scale_x_log10(breaks = unique(NCA$DOSE)) +
    xgx_scale_y_log10() +
    labs(x = dose_label, y = concnorm_label)
}

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=5--------------
if (!exists("NCA")) {
  warning("For covariate exploration, it is highly recommended to perform an NCA")
} else {
  NCA_cts <- NCA[, c("PARAM", "VALUE", check$cts_covariates$Covariate)] %>%
    tidyr::gather(COV, COV_VALUE, -c(PARAM, VALUE))
  
  NCA_cat <- NCA[, c("PARAM", "VALUE", check$cat_covariates$Covariate)] %>%
    tidyr::gather(COV, COV_VALUE, -c(PARAM, VALUE))
  
  if (nrow(check$cts_covariates) >= 1) {
    gg <- ggplot(data = NCA_cts, aes(x = COV_VALUE, y = VALUE)) +
      geom_point() +
      geom_smooth(method = "loess", color = "black") +
      facet_grid(PARAM~COV,switch = "y", scales = "free_y") +
      xgx_scale_x_log10() +
      xgx_scale_y_log10() +
      labs(x = "Covariate Value", y = "NCA Parameter Value")
    print(gg)
  }
  
  if (nrow(check$cat_covariates) >= 1) {
    gg <- ggplot(data = NCA_cat, aes(x = COV_VALUE, y = VALUE)) +
      geom_boxplot() +
      facet_grid(PARAM~COV, switch = "y", scales = "free_y") +
      xgx_scale_y_log10() +
      labs(x = "Covariate Value", y = "NCA Parameter Value")
    print(gg)
  }
}

