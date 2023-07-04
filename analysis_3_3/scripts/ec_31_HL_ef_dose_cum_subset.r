# Author:        Francesc Badia
# Date:         2018-01-17 15:06:37
# ------------------------------
# Description:  epict analysis: linERR to subsets cohort all_excl_therap_syndrel
# Appendix Table 3: Linear ERR/100 mGy by type of haematological neoplasm and by country - adjusted for age, sex, birth cohort
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping")
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')

# function to save fit results
source("analysis_UKOrigCohort/analysis_HL_cat/scripts/f_results_fit_2_jf.r")
 
dt <- readRDS("transformed_cohorts/dt1_new_outcomes_v2.rds")

dt$cumcat3_short <- 1+
  as.numeric(dt$dose_cum>=5)+
  as.numeric(dt$dose_cum>=10)+
  as.numeric(dt$dose_cum>=15)+
  as.numeric(dt$dose_cum>=20)


lag <- 2

output_path     <- "analysis_3_3/results/"
output_filename <- "ec_res_HL_ef_dose_cum_subsets.xlsx"
outcome_name    <- "HL"
data_set_format <- "event_fromat"


subsets <- c("UK","all")
susbsets_ind <- list(uk=c(19),all=c(11:19))

results <- data.frame()

for(i in 1:length(subsets))
{
  # subset
  dt1 <- dt[which(dt$country %in% susbsets_ind[[i]]),]
  # n cases by exp category
  dt1$cumcat3_short <- as.factor(dt1$cumcat3_short)
  temp <- dt1[dt1$n_pe==0 & dt1$HL==1,]
  temp <- temp %>% group_by(cumcat3_short) %>% summarise(n=n()) %>% complete(cumcat3_short, fill = list(n = 0))
  
  # set the formula for the model
  formula <- Surv(entry_age,exit_age,HL)~loglin(factor(cumcat3_short))+strata(sex,country,birthcohort)
  
  # fit the model
  fit <- try(f_fit_linERR_all(formula,dt1,id_name="patientids",time_name="age",lag=lag))
  
  # save results
  results <-  f_results_fit_2_cumcat2short(fit, temp)
}

results <- rbind(results_UK, results_All)

write.table(results,paste0(output_path,output_filename),row.names = F)

write_xlsx(results,paste0(output_path,output_filename))
