# Author:        Francesc Badia
# Date:         2018-01-17 15:06:37
# ------------------------------
# Description:  epict analysis: linERR to all cohort Lymphoid
# Excluding people with 1% highest cum dose
# ------------------------------

rm(list=ls())
setwd('Y:/EPI CT Analysis/2022_new_grouping/')
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')

# function to save fit results
source("analysis_17/scripts/f_results_fit_1.r")

dt1 <- readRDS("transformed_cohorts/dt1_new_outcomes_v2.rds")
lag <- 2

# exclude those subjects with cum exposure > 99%
dt2 <- dt1[dt1$n_pe==0, c("dose_cum")]
pct99 <- quantile(dt2$dose_cum, c(0.99))
dt2 <- dt1[dt1$n_pe==0 & dt1$dose_cum<pct99,]

dt1 <- dt1[dt1$patientids %in% dt2$patientids,]
rm(dt2)

output_path     <- "analysis_17/results/"
output_filename <- "ec_res_Lymphoid_ef_dose_cum_all.csv"
outcome_name    <- "Lymphoid"
data_set_format <- "event_fromat"


# set the formula for the model
formula <- Surv(entry_age,exit_age,Lymphoid)~lin(dose_cum)+strata(sex,country,birthcohort)

# fit the model
fit <- f_fit_linERR_all(formula,dt1,id_name="patientids",time_name="age",lag=lag)

# save results
res <- f_results_fit_1(fit)

write.table(t(as.data.frame(res)),paste0(output_path,output_filename),row.names = F)
