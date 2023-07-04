# Author:        Francesc Badia
# Date:         2018-01-17 15:06:37
# ------------------------------
# Description:  epict analysis: linERR adj ses to ses countries Lymphoid
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping")
source('rERR_ModifiedFunctions/rERR_jf.r')
library(dplyr)

# function to save the fit results
source("analysis_5/scripts/f_results_fit_5.r")

dt1 <- readRDS("transformed_cohorts/dt1_new_outcomes.rds")
lag <- 2

output_path     <- "analysis_5/results/"
output_filename <- "ec_res_Lymphoid_ef_dose_cum_ses_adj_ses_count.csv"
outcome_name    <- "Lymphoid"
data_set_format <- "event_fromat"

ses_countries <- c(11, 13, 15, 17)
dt1 <- dt1[which(dt1$country %in% ses_countries),]
plyr::count(distinct(data.frame(patientids=dt1$patientids,ses=dt1$sesorig2))$ses)

# set the formula for the model
formula1 <- Surv(entry_age,exit_age,Lymphoid)~lin(dose_cum)+strata(sex,country,birthcohort)
formula2 <- Surv(entry_age,exit_age,Lymphoid)~loglin(factor(sesorig2))+lin(dose_cum)+strata(sex,country,birthcohort)

# fit the model
fit1 <- try(f_fit_linERR_all(formula1,dt1,id_name="patientids",time_name="age",lag=lag))
fit2 <- try(f_fit_linERR_all(formula2,dt1,id_name="patientids",time_name="age",lag=lag))

summary(fit1)
summary(fit2)

# save results
res <- f_results_fit_5(fit1,fit2)

res <- as.data.frame(t(res))

write_xlsx(res,paste0(output_path,output_filename))
