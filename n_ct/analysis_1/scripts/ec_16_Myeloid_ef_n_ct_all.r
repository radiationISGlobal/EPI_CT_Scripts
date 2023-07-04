# Author:        Francesc Badia
# Date:         2018-01-17 15:06:37
# ------------------------------
# Description:  epict analysis: linERR to all cohort Myeloid
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping/")
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')

# function to save fit results
source("analysis_1/scripts/f_results_fit_1.r")

dt1 <- readRDS("transformed_cohorts/dt1_ef_n_ct_new_outcomes_v2.rds")
lag <- 2

output_path     <- "n_ct/analysis_1/results/"
output_filename <- "ec_res_Myeloid_ef_n_ct_all.csv"
outcome_name    <- "Myeloid"
data_set_format <- "event_fromat"


# set the formula for the model
formula <- Surv(entry_age,exit_age,Myeloid)~lin(n_ct)+strata(sex,country,birthcohort)

# fit the model
fit <- f_fit_linERR_all(formula,dt1,id_name="patientids",time_name="age",lag=lag)

# save results
res <- f_results_fit_1(fit)

write.table(t(as.data.frame(res)),paste0(output_path,output_filename),row.names = F)
