# Author:        Francesc Badia
# Date:         2018-01-22
# ------------------------------
# Description:  epict analysis: linERR to all cohort all_excl_therap_syndrel
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping")
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')

# function to save the fit results
source("analysis_7/scripts/f_results_fit_7_jf.r")
 
dt1 <- readRDS("transformed_cohorts/dt1_ef_grouped_doses_yearsBefdiag.rds")
lag <- 2

output_path     <- "analysis_7/results/"
output_filename <- "ec_res_all_excl_therap_syndrel_ef_gr_dose_all.xlsx"
outcome_name    <- "all_excl_therap_syndrel"
data_set_format <- "event_fromat"

n1 <- nrow(dt1[dt1$n_pe==0 & dt1$cdose_0_4>0 & dt1$all_excl_therap_syndrel==1,])
n2 <- nrow(dt1[dt1$n_pe==0 & dt1$cdose_5_9>0 & dt1$all_excl_therap_syndrel==1,])
n3 <- nrow(dt1[dt1$n_pe==0 & dt1$cdose_10_>0 & dt1$all_excl_therap_syndrel==1,])

# set the formula for the model
#formula <- Surv(entry_age,exit_age,all_excl_therap_syndrel)~lin(cdose_0_4,cdose_5_9,cdose_10_)+strata(sex,country,birthcohort)
#formula <- Surv(entry_age,exit_age,all_excl_therap_syndrel)~lin(cdose_0_4)+strata(sex,country,birthcohort)
#formula <- Surv(entry_age,exit_age,all_excl_therap_syndrel)~lin(cdose_5_9)+strata(sex,country,birthcohort)
formula <- Surv(entry_age,exit_age,all_excl_therap_syndrel)~lin(cdose_10_)+strata(sex,country,birthcohort)

# fit the model
fit_all_y3 <- f_fit_linERR_all(formula,dt1,id_name="patientids",time_name="age",lag=lag)

# save results
results <- f_results_fit_7(fit_all, c(n1,n2,n3))

#write.table(results,paste0(output_path,output_filename),row.names = F)
write_xlsx(results,paste0(output_path,output_filename))



# 
14102.10679 - 14117.2026074593 = 15.09582
 
1 - pchisq(15.09582, 2, lower.tail = T)

