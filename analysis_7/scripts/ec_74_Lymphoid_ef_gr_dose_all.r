# Author:        Francesc Badia
# Date:         2018-01-22
# ------------------------------
# Description:  epict analysis: linERR to all cohort Lymphoid
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping")
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')

# function to save the fit results
source("analysis_7/scripts/f_results_fit_7_jf.r")

dt1 <- readRDS("transformed_cohorts/dt2_ef_grouped_doses_new_outcomes.rds")
lag <- 2

output_path     <- "analysis_7/results/"
output_filename <- "ec_res_Lymphoid_ef_gr_dose_all.xlsx"
outcome_name    <- "Lymphoid"
data_set_format <- "event_fromat"

n1 <- nrow(dt1[dt1$n_pe==0 & dt1$cdose_0_4>0 & dt1$Lymphoid==1,])
n2 <- nrow(dt1[dt1$n_pe==0 & dt1$cdose_5_9>0 & dt1$Lymphoid==1,])
n3 <- nrow(dt1[dt1$n_pe==0 & dt1$cdose_10_>0 & dt1$Lymphoid==1,])

# set the formula for the model
formula <- Surv(entry_age,exit_age,Lymphoid)~lin(cdose_0_4,cdose_5_9,cdose_10_)+strata(sex,country,birthcohort)
#formula <- Surv(entry_age,exit_age,Lymphoid)~lin(cdose_0_4)+strata(sex,country,birthcohort)
#formula <- Surv(entry_age,exit_age,Lymphoid)~lin(cdose_5_9)+strata(sex,country,birthcohort)
#formula <- Surv(entry_age,exit_age,Lymphoid)~lin(cdose_10_)+strata(sex,country,birthcohort)


# fit the model
fit_lym <- f_fit_linERR_all(formula,dt1,id_name="patientids",time_name="age",lag=lag)

# save results
results <- f_results_fit_7(fit_lym, c(n1,n2,n3))

#write.table(results,paste0(output_path,output_filename),row.names = F)
write_xlsx(results,paste0(output_path,output_filename))



# 
10293.31855 - 10305.6619708028 = 12.34342
  
1 - pchisq(12.34342, 2, lower.tail = T)

