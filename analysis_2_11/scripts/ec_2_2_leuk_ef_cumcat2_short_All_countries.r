
#TABLES EPI-CT International haematological malignancies risk analysis_2022_08_24

# Appendix Table 9
# ------------------------------
# Description:  leukUK cumcat2_short ef all
# ------------------------------



rm(list=ls())
library(tidyr)
setwd("Y:/EPI CT Analysis/2022_new_grouping")
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')

# function to save fit results
source("analysis_2_11/scripts/f_results_fit_2_jf_fr.r")
source("analysis_2_11/scripts/f_results_fit_2_jf_uk.r")
source("analysis_2_11/scripts/f_results_fit_2_jf_NE.r")

# function: linear trend test
source("f_linear_trend_test.r")



output_path     <- "analysis_2_11/results/"
output_filename <- "ec_res_leuk_ef_cumcat2_short_All_Countries.csv"




#FRANCE
dt1_fr <- readRDS("transformed_cohorts/dt1_leuk_outcomes_Fr_restrictied.rds")


outcome_name    <- "leuk_france"
data_set_format <- "event_fromat"



lag <- 2

# n cases by exp category
dt1_fr$cumcat3_short_Fr <- as.factor(dt1_fr$cumcat3_short_Fr)
temp <- dt1_fr[dt1_fr$n_pe==0 & dt1_fr$leuk_france==1,]
temp <- temp %>% group_by(cumcat3_short_Fr) %>% summarise(n=n()) %>% complete(cumcat3_short_Fr, fill = list(n = 0))

# n cases by exp category
#temp <- unique(dt1_fr[dt1_fr$n_pe==0 & dt1_fr$Myeloid_excl_MDS_MPN==1,c("patientids", "Myeloid_excl_MDS_MPN", "cumcat2_short")])
#temp <- dt1_fr[dt1_fr$n_pe==0 & dt1_fr$all_excl_therap_syndrel==1,]
#temp <- temp %>% group_by(cumcat2_short) %>% summarise(n=n())

# set the formula for the model
formula <- Surv(entry_age,exit_age,leuk_france)~loglin(factor(cumcat3_short_Fr))+strata(sex,country,birthcohort)

# fit the model
fit <- f_fit_linERR_all(formula,dt1_fr,id_name="patientids",time_name="age",lag=lag)

# save results
results_Fr <- f_results_fit_2_cumcat2short_fr(fit, temp)

write_xlsx(results_Fr,"analysis_2_11/results/ec_res_leuk_ef_cumcat2_short_All_Countries_Fr_restrictions.xlsx")




dt1 <- readRDS("transformed_cohorts/dt1_leuk_outcomes_Fr_UK_NE.rds")



dt1$cumcat3_short_UK <- 1+
  as.numeric(dt1$dose_cum>=5)+
  as.numeric(dt1$dose_cum>=10)+
  as.numeric(dt1$dose_cum>=15)+
  as.numeric(dt1$dose_cum>=20)+
  as.numeric(dt1$dose_cum>=30)

dt1$cumcat3_short_NE <- 1+
  as.numeric(dt1$dose_cum>=5)+
  as.numeric(dt1$dose_cum>=10)+
  as.numeric(dt1$dose_cum>=17)


lag <- 2




#UK
outcome_name    <- "leuk_UK"
data_set_format <- "event_fromat"

# n cases by exp category
dt1$cumcat3_short_UK <- as.factor(dt1$cumcat3_short_UK)
temp <- dt1[dt1$n_pe==0 & dt1$leuk_UK==1,]
temp <- temp %>% group_by(cumcat3_short_UK) %>% summarise(n=n()) %>% complete(cumcat3_short_UK, fill = list(n = 0))

lag <- 2
# n cases by exp category
#temp <- unique(dt1[dt1$n_pe==0 & dt1$Myeloid_excl_MDS_MPN==1,c("patientids", "Myeloid_excl_MDS_MPN", "cumcat2_short")])
#temp <- dt1[dt1$n_pe==0 & dt1$all_excl_therap_syndrel==1,]
#temp <- temp %>% group_by(cumcat2_short) %>% summarise(n=n())

# set the formula for the model
formula <- Surv(entry_age,exit_age,leuk_UK)~loglin(factor(cumcat3_short_UK))+strata(sex,country,birthcohort)

# fit the model
fit <- f_fit_linERR_all(formula,dt1,id_name="patientids",time_name="age",lag=lag)

# save results
results_UK <- f_results_fit_2_cumcat2short_uk(fit, temp)

#write_xlsx(results,paste0(output_path,output_filename))


#NE
outcome_name    <- "leuk_NE"
data_set_format <- "event_fromat"

# n cases by exp category
dt1$cumcat3_short_NE <- as.factor(dt1$cumcat3_short_NE)
temp <- dt1[dt1$n_pe==0 & dt1$leuk_NE==1,]
temp <- temp %>% group_by(cumcat3_short_NE) %>% summarise(n=n()) %>% complete(cumcat3_short_NE, fill = list(n = 0))

# n cases by exp category
#temp <- unique(dt1[dt1$n_pe==0 & dt1$Myeloid_excl_MDS_MPN==1,c("patientids", "Myeloid_excl_MDS_MPN", "cumcat2_short")])
#temp <- dt1[dt1$n_pe==0 & dt1$all_excl_therap_syndrel==1,]
#temp <- temp %>% group_by(cumcat2_short) %>% summarise(n=n())

# set the formula for the model
formula <- Surv(entry_age,exit_age,leuk_NE)~loglin(factor(cumcat3_short_NE))+strata(sex,country,birthcohort)

# fit the model
fit <- f_fit_linERR_all(formula,dt1,id_name="patientids",time_name="age",lag=lag)

# save results
results_NE <- f_results_fit_2_cumcat2short_NE(fit, temp)

#write_xlsx(results,paste0(output_path,output_filename))

results <- rbind(results_Fr, results_UK, results_NE)

write_xlsx(results,paste0(output_path,output_filename))

write_xlsx(results,"results/epict_analysis_2_11_linERR_leuk_All_countries.xlsx")

