library(ggplot2)
library(xgx)
context("Data Check")

data_missing = data.frame(x = 1)

# data = data.frame    (ID   = 1,
#                       EVID = 0,
#                       AMT  = 0,
#                       TIME = c(0,1,1),
#                       DV   = c(0,5,6),
#                       YTYPE= 2)
# 
# check = xgx_check_data(data)

test_that("xgx_check_data throws correct errors", {
  expect_error(xgx_check_data(data_missing),
               "These columns must be present in the dataset: ID,EVID,AMT,TIME,DV,YTYPE")
})
