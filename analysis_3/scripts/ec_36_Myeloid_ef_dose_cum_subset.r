# Author:        Francesc Badia
# Date:         2018-01-17 15:06:37
# ------------------------------
# Description:  epict analysis: linERR to subsets cohort Myeloid
# Appendix Table 3: Linear ERR/100 mGy by type of haematological neoplasm and by country - adjusted for age, sex, birth cohort

# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping")
source('Y:/EPI CT Analysis/2022_new_grouping/rERR_ModifiedFunctions/rERR_jf.r')

# function to save fit results
source("analysis_3/scripts/f_results_fit_3.r")

dt <- readRDS("transformed_cohorts/dt1_new_outcomes.rds")
lag <- 2

output_path     <- "analysis_3/results/"
output_filename <- "ec_res_Myeloid_ef_dose_cum_subsets.csv"
outcome_name    <- "Myeloid"
data_set_format <- "event_fromat"

#subsets <- c("UK","all_except_UK","France","The_Netherlands","Norway","Sweden","Others")
#susbsets_ind <- list(uk=c(19),all_except_uk=c(11:18),france=c(13),the_netherlands=c(15),norway=c(16),sweden=c(18),others=c(11,12,14,17))
subsets <- c("UK","all_except_UK","France","The_Netherlands","Norway","Sweden","Others", 
             "all_except_Bel", "all_except_Den", "all_except_Fr", "all_except_Ger", 
             "all_except_Net", "all_except_Nor", "all_except_Sp", "all_except_Sw")
susbsets_ind <- list(uk=c(19),all_except_uk=c(11:18),france=c(13),the_netherlands=c(15),norway=c(16),sweden=c(18),others=c(11,12,14,17),
                     all_except_Bel=c(12:19), all_except_Den=c(11,13:19), all_except_Fr=c(11:12,14:19), all_except_Ger=c(11:13,15:19), 
                     all_except_Net=c(11:14,16:19), all_except_Nor=c(11:15,17:19), all_except_Sp=c(11:16,18:19), all_except_Sw=c(11:17,19))

results <- data.frame()

for(i in 1:length(subsets))
{
  # subset
  dt1 <- dt[which(dt$country %in% susbsets_ind[[i]]),]

  # set the formula for the model
  formula <- Surv(entry_age,exit_age,Myeloid)~lin(dose_cum)+strata(sex,country,birthcohort)
  
  # fit the model
  fit <- try(f_fit_linERR_all(formula,dt1,id_name="patientids",time_name="age",lag=lag))
  
  # save results
  results <- f_results_fit_3(fit)
}
write.table(results,paste0(output_path,output_filename),row.names = F)

