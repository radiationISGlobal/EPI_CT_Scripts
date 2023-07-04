# Author:        Francesc Badia
# Date:         2018-01-18 12:04:07
# ------------------------------
# Description:  leuk_noCLL cumcat2_short ef all
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping/")
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')

# function to save fit results
source("n_ct/analysis_2/scripts/f_results_fit_2.r")

# function: linear trend test
source("f_linear_trend_test.r")

dt1 <- readRDS("transformed_cohorts/dt1_ef_n_ct_new_outcomes_v2.rds")
lag <- 2

output_path     <- "n_ct/analysis_2/results/"
output_filename <- "ec_res_Myeloid_ef_cumcat2_short_all.csv"
outcome_name    <- "Myeloid"
data_set_format <- "event_fromat"

# n cases by exp category
dt1$n_ct_cat <- as.factor(dt1$n_ct_cat)
temp <- dt1[dt1$n_pe==0 & dt1$Myeloid==1,]
temp <- temp %>% group_by(n_ct_cat) %>% summarise(n=n()) %>% complete(n_ct_cat, fill = list(n = 0))

# set the formula for the model
formula <- Surv(entry_age,exit_age,Myeloid)~loglin(factor(n_ct_cat))+strata(sex,country,birthcohort)

# fit the model
fit <- f_fit_linERR_all(formula,dt1,id_name="patientids",time_name="age",lag=lag)

# save results
results <- f_results_fit_2(fit, temp)

write.table(results,paste0(output_path,output_filename),row.names = F)

