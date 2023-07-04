
#TABLES EPI-CT International haematological malignancies risk analysis_2022_08_24

# Table 3 - RR and 95% CI per active bone marrow dose category and ERR/100 mGy by type of myeloiid neoplasm
# - analyses stratified on sex, birth cohort and country
# ------------------------------
# Description:  AML_AL_ALMP_AL_excl_gen cumcat2_short ef all
# ------------------------------



rm(list=ls())
library(tidyr)
setwd("Y:/EPI CT Analysis/2022_new_grouping")
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')

# function to save fit results
source("analysis_2_4/scripts/f_results_fit_2_jf.r")

# function: linear trend test
source("f_linear_trend_test.r")

dt1 <- readRDS("transformed_cohorts/dt1_new_outcomes_v2.rds")
lag <- 2

output_path     <- "analysis_2_4/results/"
output_filename <- "ec_res_AML_AL_ALMP_AL_excl_gen_ef_cumcat2_short_all.xlsx"
outcome_name    <- "AML_AL_ALMP_AL_excl_gen"
data_set_format <- "event_fromat"

# n cases by exp category
dt1$cumcat2_short <- as.factor(dt1$cumcat2_short)
temp <- dt1[dt1$n_pe==0 & dt1$AML_AL_ALMP_AL_excl_gen==1,]
temp <- temp %>% group_by(cumcat2_short) %>% summarise(n=n()) %>% complete(cumcat2_short, fill = list(n = 0))

# n cases by exp category
#temp <- unique(dt1[dt1$n_pe==0 & dt1$Myeloid_excl_MDS_MPN==1,c("patientids", "Myeloid_excl_MDS_MPN", "cumcat2_short")])
#temp <- dt1[dt1$n_pe==0 & dt1$all_excl_therap_syndrel==1,]
#temp <- temp %>% group_by(cumcat2_short) %>% summarise(n=n())

# set the formula for the model
formula <- Surv(entry_age,exit_age,AML_AL_ALMP_AL_excl_gen)~loglin(factor(cumcat2_short))+strata(sex,country,birthcohort)

# fit the model
fit <- f_fit_linERR_all(formula,dt1,id_name="patientids",time_name="age",lag=lag)

# save results
results <- f_results_fit_2_cumcat2short(fit, temp)

write_xlsx(results,paste0(output_path,output_filename))
