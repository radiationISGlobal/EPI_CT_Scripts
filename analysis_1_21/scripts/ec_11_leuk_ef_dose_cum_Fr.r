#TABLES EPI-CT International haematological malignancies risk analysis_2022_08_24

# Table 2: Relative risk (RR) and 95% CI per cumulative active bone marrow dose category and ERR/100 mGy by type of haematological malignancies
# - analyses stratified on sex, birth cohort and country
# ------------------------------
# Description:  leuk_france 
# ------------------------------



rm(list=ls())
library(tidyr)
setwd("Y:/EPI CT Analysis/2022_new_grouping")
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')


# function to save fit results
source("analysis_1/scripts/f_results_fit_1.r")
 
dt1 <- readRDS("transformed_cohorts/dt1_leuk_outcomes_Fr_UK_NE.rds")
dt1_1 <- dt1[which(dt1$country == 13),]

dt1_fr <- dt1_1 %>% group_by(patientids) %>% filter(all(age < 15))

lag <- 2

output_path     <- "analysis_1_21/results/"
output_filename <- "ec_res_leuk_ef_dose_cum_Fr.xlsx"
outcome_name    <- "leuk_france"
data_set_format <- "event_fromat"

# set the formula for the model
formula <- Surv(entry_age,exit_age,leuk_france)~lin(dose_cum)+strata(sex,country,birthcohort)

# fit the model
fit <- f_fit_linERR_all(formula,dt1_fr,id_name="patientids",time_name="age",lag=lag)

# save results
res <- f_results_fit_1(fit)
res <- as.data.frame(t(res))

write_xlsx(res,paste0(output_path,output_filename))
