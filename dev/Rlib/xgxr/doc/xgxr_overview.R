## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7
)

## ---- message=FALSE------------------------------------------------------
library(xgxr)
library(ggplot2)
library(dplyr)

## ------------------------------------------------------------------------
# xgx_create_rmarkdown(type = "pk", open_file = FALSE)

## ---- fig.height=7-------------------------------------------------------
#if (sessionInfo()$otherPkgs$ggplot2$Version == "2.2.1") {
#   nsubj <- 50
#   ntime <- 8
#   time <- rep(c(1, 2, 4, 8, 12, 24, 36, 48), nsubj)
#   id <- sort(rep(seq(1, nsubj), ntime))
#   trt <- sort(rep(c(25, 50, 100, 150, 300), ntime * nsubj / 5))
#   ka <- rep(rlnorm(nsubj, -0.5, 0.3), each = ntime)
#   ke <- rep(rlnorm(nsubj, -3, 0.3), each = ntime)
#   conc <- trt * (ka * ke / (ka - ke)) * (exp(-time * ke) - exp(-time * ka)) * (rep(stats::rlnorm(ntime * nsubj, 0.3, 0.1)))
 
#   data <- data.frame(TIME = time, CONC = conc, ID = id, TRT = trt)
#   xgx_PK_summary(data = data, labels = list(TRT = "Dose"),
#                  units_dataset = list(TIME = "Hours", CONC = "ng/mL", TRT = "mg"))
#} else {
#  print("Currently only works with ggplot2 version 2.2.1 (on DaVinci), and not version 3")
#}

## ------------------------------------------------------------------------
dirs <- list(
  parent_dir = tempdir(),
  rscript_dir = tempdir(),
  rscript_name = "example.R",
  results_dir = tempdir(),
  filename_prefix = "example_")
data <- data.frame(x = 1:1000, y = stats::rnorm(1000))
g <- xgx_plot(data = data, aes(x = x, y = y)) +
  geom_point()
xgx_save(width = 4, height = 4, dirs = dirs, filename_main = "example_plot", status = "DRAFT")

## ------------------------------------------------------------------------
data <- data.frame(x = 1:1000, y = stats::rnorm(1000))
g <- xgx_plot(data = data, aes(x = x, y = y)) +
  geom_point()
filename = file.path(tempdir(), "png_example.png")
ggsave(filename, plot = g, height = 4, width = 4, dpi = 75)
xgx_annotate_status_png(filename, "./ExampleScript.R")

## ------------------------------------------------------------------------
x <- data.frame(ID = c(1, 2), SEX = c("male", "female"))
data <- xgx_save_table(x, dirs = dirs, filename_main = "ExampleTable")
knitr::kable(data)

## ------------------------------------------------------------------------
xgx_plot(mtcars, aes(x = cyl, y = mpg)) + geom_point()

## ------------------------------------------------------------------------
theme_set(xgx_theme())

## Alternative, equivalent function:
xgx_theme_set()

## ---- fig.width=4, fig.height=2------------------------------------------
# time <- rep(seq(1,10),5)
# id <- sort(rep(seq(1,5), 10))
# conc <- exp(-time)*sort(rep(rlnorm(5),10))
# 
# data <- data.frame(time = time, concentration  = conc, id = factor(id))
# xgx_plot() + xgx_geom_spaghetti(data = data, mapping = aes(x = time, y = concentration, group = id, color = id))
# 
# xgx_spaghetti(data = data, mapping = aes(x = time, y = concentration, group = id, color = id))

## ---- fig.width=4, fig.height=2------------------------------------------
data <- data.frame(x = rep(c(1, 2, 3), each = 20),
                   y = rep(c(1, 2, 3), each = 20) + stats::rnorm(60),
                   group = rep(1:3, 20))

xgx_plot(data,aes(x = x, y = y)) +
    xgx_stat_ci(conf_level = .95)

xgx_plot(data,aes(x = x, y = y)) +
    xgx_stat_ci(conf_level = .95, geom = list("pointrange","line"))

xgx_plot(data,aes(x = x, y = y)) +
    xgx_stat_ci(conf_level = .95, geom = list("ribbon","line"))

xgx_plot(data,aes(x = x, y = y, group = group, color = factor(group))) + 
    xgx_stat_ci(conf_level = .95, alpha =  0.5,
                position = position_dodge(width = 0.5))

## ---- fig.width=4, fig.height=2------------------------------------------
# plotting lognormally distributed data
data <- data.frame(x = rep(c(1, 2, 3), each = 20),
                   y = 10^(rep(c(1, 2, 3), each = 20) + stats::rnorm(60)),
                   group = rep(1:3, 20))
xgx_plot(data, aes(x = x, y = y)) + 
 xgx_stat_ci(conf_level = 0.95, distribution = "lognormal")
 
# note: you DO NOT need to use both distribution = "lognormal" and scale_y_log10()
xgx_plot(data,aes(x = x, y = y)) + 
 xgx_stat_ci(conf_level = 0.95) + xgx_scale_y_log10()

# plotting binomial data
data <- data.frame(x = rep(c(1, 2, 3), each = 20),
                  y = rbinom(60, 1, rep(c(0.2, 0.6, 0.8), each = 20)),
                  group = rep(1:3, 20))
xgx_plot(data, aes(x = x, y = y)) + 
 xgx_stat_ci(conf_level = 0.95, distribution = "binomial")

## ------------------------------------------------------------------------
df <- data.frame(x = c(0, stats::rlnorm(1000, 0, 1)),
                 y = c(0, stats::rlnorm(1000, 0, 3)))
xgx_plot(data = df, aes(x = x, y = y)) + 
  geom_point() + 
  xgx_scale_x_log10() + 
  xgx_scale_y_log10()

## ---- fig.height=3.5, warning=FALSE--------------------------------------
conc <- 10^(seq(-3, 3, by = 0.1))
ec50 <- 1
data <- data.frame(concentration = conc, 
                  bound_receptor = 1 * conc / (conc + ec50))
gy <- xgx_plot(data, aes(x = concentration, y = bound_receptor)) + 
  geom_point() + 
  geom_line() + 
  xgx_scale_x_log10() +
  xgx_scale_y_reverselog10()

gx <- xgx_plot(data, aes(x = bound_receptor, y = concentration)) + 
  geom_point() + 
  geom_line() + 
  xgx_scale_y_log10() +
  xgx_scale_x_reverselog10()

gridExtra::grid.arrange(gy, gx, nrow = 1)

## ---- fig.height=7-------------------------------------------------------
data <- data.frame(x = 1:1000, y = stats::rnorm(1000))
g <- xgx_plot(data = data, aes(x = x, y = y)) + 
  geom_point()
g1 <- g + xgx_scale_x_time_units(units_dataset = "hours", units_plot = "hours")
g2 <- g + xgx_scale_x_time_units(units_dataset = "hours", units_plot = "days")
g3 <- g + xgx_scale_x_time_units(units_dataset = "hours", units_plot = "weeks")
g4 <- g + xgx_scale_x_time_units(units_dataset = "hours", units_plot = "months")

gridExtra::grid.arrange(g1, g2, g3, g4, nrow = 2)

## ---- message=FALSE------------------------------------------------------
data <- mad_missing_duplicates %>%
  filter(CMT %in% c(1, 2, 3)) %>%
  rename(DV      = LIDV,
         YTYPE   = CMT,
         USUBJID = ID)
covariates <- c("WEIGHTB", "SEX")
check <- xgx_check_data(data, covariates)

knitr::kable(check$summary)
knitr::kable(head(check$data_subset))

## ------------------------------------------------------------------------
covar <- xgx_summarize_covariates(data,covariates)
knitr::kable(covar$cts_covariates)
knitr::kable(covar$cat_covariates)

