
#TABLES EPI-CT International haematological malignancies risk analysis_2022_08_24

# Appendix Table 9
# ------------------------------
# Description:  leukUK cumcat3_short ef all
# ------------------------------



rm(list=ls())
library(tidyr)
setwd("Y:/EPI CT Analysis/2022_new_grouping")
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')

# function to save fit results
source("analysis_2_11/scripts/f_results_fit_2_jf_NE.r")

# function: linear trend test
source("f_linear_trend_test.r")

dt1 <- readRDS("transformed_cohorts/dt1_leuk_outcomes_Fr_UK_NE.rds")
dt1_1 <- dt1[which(dt1$country == 15),]

dt1_1$cumcat3_short <- 1+
  as.numeric(dt1_1$dose_cum>=5)+
  as.numeric(dt1_1$dose_cum>=10)+
  as.numeric(dt1_1$dose_cum>=17)


lag <- 2

output_path     <- "analysis_2_11/results/"
output_filename <- "ec_res_leuk_ef_cumcat3_short_NE.xlsx"
outcome_name    <- "leuk_NE"
data_set_format <- "event_fromat"

# n cases by exp category
dt1_1$cumcat3_short <- as.factor(dt1_1$cumcat3_short)
temp <- dt1_1[dt1_1$n_pe==0 & dt1_1$leuk_NE==1,]
temp <- temp %>% group_by(cumcat3_short) %>% summarise(n=n()) %>% complete(cumcat3_short, fill = list(n = 0))

# n cases by exp category
#temp <- unique(dt1[dt1$n_pe==0 & dt1$Myeloid_excl_MDS_MPN==1,c("patientids", "Myeloid_excl_MDS_MPN", "cumcat3_short")])
#temp <- dt1[dt1$n_pe==0 & dt1$all_excl_therap_syndrel==1,]
#temp <- temp %>% group_by(cumcat3_short) %>% summarise(n=n())

# set the formula for the model
formula <- Surv(entry_age,exit_age,leuk_NE)~loglin(factor(cumcat3_short))+strata(sex,country,birthcohort)

# fit the model
fit <- f_fit_linERR_all(formula,dt1_1,id_name="patientids",time_name="age",lag=lag)

# save results
results <- f_results_fit_2_cumcat2short_NE(fit, temp)

write_xlsx(results,paste0(output_path,output_filename))

