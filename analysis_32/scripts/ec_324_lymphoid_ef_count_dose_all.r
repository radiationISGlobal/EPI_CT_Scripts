# Author:        Francesc Badia
# Date:         2018-01-22
# ------------------------------
# Description:  epict analysis: linERR to all cohort Lymphoid by sex
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping")
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')


# function to save the fit results
source("analysis_32/scripts/f_results_fit_9.r")

dt1 <- readRDS("transformed_cohorts/dt1_new_outcomes.rds")
lag <- 2

output_path     <- "analysis_32/results/"
output_filename <- "ec_res_Lymphoid_ef_count_dose_all_bysex.xlsx"
outcome_name    <- "Lymphoid"
data_set_format <- "event_fromat"

# define variables for sex doses
dt1$cdose_male   <- ifelse(dt1$sex==1,dt1$dose_cum,0)
dt1$cdose_female <- ifelse(dt1$sex==2,dt1$dose_cum,0)

# set the formula for the model
formula <- Surv(entry_age,exit_age,Lymphoid)~lin(cdose_male,cdose_female)+strata(sex,country,birthcohort)
#formula <- Surv(entry_age,exit_age,Lymphoid)~lin(cdose_male)+strata(sex,country,birthcohort)
formula <- Surv(entry_age,exit_age,Lymphoid)~lin(cdose_male)+strata(sex,country,birthcohort)

# fit the model
fit_m_lym <- try(f_fit_linERR_all(formula,dt1,id_name="patientids",time_name="age",lag=lag))

# save results
results <- f_results_fit_9(fit)


write_xlsx(results,paste0(output_path,output_filename))
write.table(results,paste0(output_path,output_filename),row.names = F)


  
10305.62383 - 10305.6619708028  = 0.0381408
1 - pchisq( 0.0381408, 1, lower.tail = T)

