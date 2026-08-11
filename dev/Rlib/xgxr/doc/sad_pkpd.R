## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7
)

## ---- warning=FALSE, message=FALSE---------------------------------------
library(xgxr)
library(ggplot2)
library(dplyr)
library(tidyr)

# flag for labeling figures as draft
status <- "DRAFT"
 
# ggplot settings
xgx_theme_set()

## ---- warning=FALSE, message=FALSE---------------------------------------
pkpd_data <- case1_pkpd %>%
  arrange(DOSE) %>%
  select(-IPRED) %>%
  mutate(TRTACT_low2high = factor(TRTACT, levels = unique(TRTACT)),
         TRTACT_high2low = factor(TRTACT, levels = rev(unique(TRTACT))),
         DAY_label = paste("Day", PROFDAY),
         DAY_label = ifelse(DAY_label == "Day 0","Baseline",DAY_label))
 
pk_data <- pkpd_data %>%
  filter(CMT == 2)

pk_data_cycle1 <- pk_data %>%
  filter(CYCLE == 1)
 
pd_data <- pkpd_data %>%
  filter(CMT == 3)

pd_data_baseline_day85 <- pkpd_data %>%
  filter(CMT == 3,
         DAY_label %in% c("Baseline", "Day 85"))

pk_vs_pd_data <- pkpd_data %>%
  filter(!is.na(LIDV)) %>%
  select(-c(EVENTU,NAME)) %>%
  spread(CMT,LIDV) %>%
  rename(Concentration = `2`, Response = `3`)

NCA <- pk_data_cycle1 %>%
  group_by(ID, DOSE) %>%
  filter(!is.na(LIDV)) %>%
  summarize(AUC_last = caTools::trapz(TIME, LIDV),
            Cmax = max(LIDV)) %>%
  tidyr::gather(PARAM,VALUE,-c(ID, DOSE)) %>%
  ungroup() %>%
  mutate(VALUE_NORM = VALUE / DOSE)

AUC_last <- NCA %>%
  filter(PARAM == "AUC_last") %>%
  rename(AUC_last = VALUE) %>%
  select(-c(DOSE,PARAM,VALUE_NORM))

pk_vs_pd_data_day85 <- pk_vs_pd_data %>%
  filter(DAY_label == "Day 85",
         !is.na(Concentration),
         !is.na(Response)) %>%
  left_join(AUC_last)


time_units_dataset <- "hours"
time_units_plot    <- "days"
trtact_label       <- "Dose"
dose_label         <- "Dose (mg)"
conc_label         <- "Concentration (ng/ml)" 
auc_label          <- "AUCtau (h.(ng/ml))"
concnorm_label     <- "Normalized Concentration (ng/ml)/mg"
sex_label          <- "Sex"
w100_label         <- "WEIGHTB>100"
pd_label           <- "FEV1 (mL)"
cens_label         <- "Censored"

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = pk_data_cycle1, aes(x     = NOMTIME,
                                  y     = LIDV,
                                  group = DOSE,
                                  color = TRTACT_high2low)) +
  xgx_geom_ci(conf_level = 0.95) +
  xgx_scale_y_log10() +
  xgx_scale_x_time_units(units_dataset = time_units_dataset, units_plot = time_units_plot) +
  labs(y = conc_label, color = trtact_label) +
  xgx_annotate_status(status)

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = pk_data_cycle1, aes(x = NOMTIME, y = LIDV)) +
  geom_line(aes(group = ID), color = rgb(0.5, 0.5, 0.5), size = 1, alpha = 0.3) +
  scale_shape_manual(values = c(1, 8)) +
  scale_color_manual(values = c("grey50", "red")) +
  xgx_geom_ci(aes(x = NOMTIME, color = NULL, group = NULL), conf_level = 0.95) +
  xgx_scale_y_log10() +
  xgx_scale_x_time_units(units_dataset = time_units_dataset, units_plot = time_units_plot) +
  labs(y = conc_label, color = trtact_label) +
  theme(legend.position = "none") +
  facet_grid(.~TRTACT_low2high) +
  xgx_annotate_status(status)

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = pk_data_cycle1,
       aes(x = NOMTIME,
           y = LIDV / as.numeric(as.character(DOSE)),
           group = DOSE,
           color = TRTACT_high2low)) +
  xgx_geom_ci(conf_level = 0.95, alpha = 0.5, position = position_dodge(1)) +
  xgx_scale_y_log10() +
  xgx_scale_x_time_units(units_dataset = time_units_dataset, units_plot = time_units_plot) +
  labs(y = concnorm_label, color = trtact_label) +
  xgx_annotate_status(status)


## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = NCA, aes(x = DOSE, y = VALUE_NORM)) + 
  geom_boxplot(aes(group = DOSE)) +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~PARAM, scales = "free_y") +
  labs(x = dose_label) +
  theme(axis.title.y = element_blank()) +
  xgx_annotate_status(status)

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = NCA[!NCA$DOSE == 3 & !NCA$DOSE == 10 , ],
       aes(x = DOSE, y = VALUE_NORM)) +
  geom_boxplot(aes(group = DOSE)) +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~PARAM, scales = "free_y") +
  labs(x = dose_label) +
  theme(axis.title.y = element_blank()) +
  xgx_annotate_status(status)

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = pk_data_cycle1, aes(x = NOMTIME,
                                   y = LIDV,
                                   group = WEIGHTB > 100,
                                   color = WEIGHTB > 100)) + 
  xgx_geom_ci(conf_level = 0.95) +
  xgx_scale_y_log10() +
  xgx_scale_x_time_units(units_dataset = time_units_dataset, units_plot = time_units_plot) +
  facet_grid(.~DOSE) +
  labs(y = conc_label, color = w100_label) +
  xgx_annotate_status(status)

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = pd_data, aes(x     = NOMTIME,
                           y     = LIDV,
                           group = DOSE,
                           color = TRTACT_high2low)) +
  xgx_geom_ci(conf_level = 0.95) +
  xgx_scale_y_log10() + 
  xgx_scale_x_time_units(units_dataset = time_units_dataset, units_plot = time_units_plot) +
  labs(y = pd_label, color = trtact_label) + 
  xgx_annotate_status(status)

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = pd_data, aes(x = NOMTIME, y = LIDV, group = ID)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  xgx_scale_y_log10() + 
  xgx_scale_x_time_units(units_dataset = time_units_dataset, units_plot = time_units_plot) +
  facet_grid(~TRTACT_low2high) +
  labs(y = pd_label, color = trtact_label) +
  xgx_annotate_status(status)

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = pd_data_baseline_day85, aes(x = DOSE,
                                          y = LIDV,
                                          group = DOSE)) +
  xgx_geom_ci(conf_level = 0.95) + 
  facet_grid(~DAY_label) +
  labs(x = dose_label, y = pd_label, color = trtact_label) +
  xgx_annotate_status(status)

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = pd_data, aes(x = DOSE, y = LIDV, group = DOSE)) +
  xgx_geom_ci(conf_level = 0.95) +
  facet_grid(~DAY_label) +
  labs(x = dose_label, y = pd_label, color = trtact_label) +
  xgx_annotate_status(status)

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
ggplot(data = pd_data_baseline_day85, aes(x = DOSE,
                                          y = LIDV,
                                          group = WEIGHTB > 100,
                                          color = WEIGHTB > 100)) +
  xgx_geom_ci(conf_level = .95) +
  facet_grid(~DAY_label) +
  labs(x = dose_label, y = pd_label, color = w100_label) +
  xgx_annotate_status(status)

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
g = ggplot(data = pk_vs_pd_data_day85, aes(x = Concentration, y = Response)) +
  geom_point(aes(color = TRTACT_high2low, shape = factor(CENS))) +
  geom_smooth(color="black",shape=NULL) +
  xgx_scale_x_log10() +
  labs(x = conc_label, y = pd_label, color = trtact_label, shape = cens_label) +
  xgx_annotate_status(status)
print(g)

## ---- echo=TRUE, warning=FALSE, message=FALSE, fig.height=3--------------
gAUC = g + 
  aes(x = AUC_last) +
  xlab(auc_label)
print(gAUC)

## ------------------------------------------------------------------------
sessionInfo()

