library(data.table)

dt <- readRDS("~/bayes-rate-consistency-output/results/hsgp-m52-lrd_COVIMOD_multi_1234/fit_summary.rds")

dt[which.min(dt$ess_bulk),]
which.min(dt[!str_detect(dt$variable, "yhat_strata"),]$ess_bulk)
