
#TABLES EPI-CT International haematological malignancies risk analysis_2022_08_24

# Table 3 - RR and 95% CI per active bone marrow dose category and ERR/100 mGy by type of lymphoid neoplasm
# - analyses stratified on sex, birth cohort and country
# ------------------------------
# Description:  All NHL 
# ------------------------------

rm(list=ls())
library(tidyr)
setwd("Y:/EPI CT Analysis/2022_new_grouping")
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')


# function to save fit results
source("analysis_1_2/scripts/f_results_fit_1.r")
 
dt1 <- readRDS("transformed_cohorts/dt1_new_outcomes.rds")
lag <- 2

output_path     <- "analysis_1_2/results/"
output_filename <- "ec_res_NHL_ef_dose_cum.xlsx"
outcome_name    <- "NHL"
data_set_format <- "event_fromat"

# set the formula for the model
formula <- Surv(entry_age,exit_age,NHL)~lin(dose_cum)+strata(sex,country,birthcohort)

# fit the model
fit <- f_fit_linERR_all(formula,dt1,id_name="patientids",time_name="age",lag=lag)

# save results
res <- f_results_fit_1(fit)
res <- as.data.frame(t(res))

write_xlsx(res,paste0(output_path,output_filename))
