# Author:        Francesc Badia
# Date:         2018-01-17 13:39:21
# ------------------------------
# Description:  save event format data sets for each outcome and data set origin
# ------------------------------
# Modification:        exclusion by date
# Modification date:  2018-07-17
# --------------
# Modification:       this file produces lag 1, lag 2 and lag 5 data files
# Modification date:  2018-10-18 15:09:36
# --------------
#
#///  INPUT
#   - Leukemia_AnalysisFile_20190604.rds: file from Jord with onw row per CT
#
#///  OUTPUT
#   - dt1_ef.rds
#   - dt1_ef_lag1.rds
#   - dt1_ef_lag5.rds
# --------------

rm(list=ls())

library(rERR)
source('Y:/EPI CT Analysis/20191218/rERR_ModifiedFunctions/rERR_jf.r')
library(dplyr)
library(lubridate)

setwd("Y:/EPI CT Analysis/20191218/")

dt_ef  <- readRDS("Y:/EPI CT Analysis/epict_cohort/20191218/Leukemia_AnalysisFile_Date.rds") # the file we got from "Leukemia_AnalysisFile.R"

# lag period
lags <- c(1, 2, 5)
namesdf <- c("dt1_ef_lag1.rds", "dt1_ef.rds", "dt1_ef_lag5.rds")

for (l in 1:length(lags)){
  lag <- lags[l]
  #### exclusion by defual Leukemia/Lymphoma is 2 years ####
  # jordi
  dt_ef <- dt_ef[dt_ef$entry < dt_ef$exit,]
  
  #### entry age and exit age ####
  dt_ef$entry_age <- as.numeric((dt_ef$entry - dt_ef$birth) / 365.25)
  dt_ef$exit_age <- as.numeric((dt_ef$exit - dt_ef$birth) / 365.25)
  
  #### clean outcome columns: NA to 0 ####
  
  # NA in outcome columns to 0
  dt_ef[,c(32:47)] <- apply(dt_ef[,c(32:47)],2,function(z){replace(z, is.na(z), 0)})
  dt_ef$y_dob <- lubridate::year(dt_ef$birth)
  
  #### birthcohort ####
  # define birthcohort
  dt_ef$birthcohort <- 1 +as.numeric(dt_ef$y_dob>=1980)+as.numeric(dt_ef$y_dob>=1985)+as.numeric(dt_ef$y_dob>=1990)+
    as.numeric(dt_ef$y_dob>=1995)+as.numeric(dt_ef$y_dob>=2000)+as.numeric(dt_ef$y_dob>=2005)
  
  # define birthcohort from MH
  dt_ef$birthcohort_mh <- 1 +as.numeric(dt_ef$y_dob>=1980)+as.numeric(dt_ef$y_dob>=1985)+as.numeric(dt_ef$y_dob>=1990)+
    as.numeric(dt_ef$y_dob>=1995)+as.numeric(dt_ef$y_dob>=2000)+as.numeric(dt_ef$y_dob>=2005)+as.numeric(dt_ef$y_dob>=2010)
  
  #### lymph-leuk_noCLL ####
  #dt_ef$lymph_no_leuk_noCLL <- ifelse(dt_ef$lymph==1 & dt_ef$leuk_noCLL==0,1,0)
  
  #### data transformation ####
  formula <- Surv(entry_age,exit_age,Lymphoid)~lin(dose_cum)+strata(sex,country,birthcohort)
  dt2_ef <- f_to_event_table_ef_all(formula,data = dt_ef,dose_name = "ActMar_mean",time_name ="age",
                                          id_name = "patientids",covars_names = c("sex","country","birthcohort"))  # n_pe is created as a fictitional variable to calculate cum_doses and exclude ct's
  
  # # save one cohort with all the outcomes in the proper way
  # dt2_ef <- dt1_ef_lymph_no_leuk_noCLL
  
  #### Categorical Exposure ####
  # add categorical exposure
  dt2_ef$cumcat2_long <- 1+
    as.numeric(dt2_ef$dose_cum>=5)+
    as.numeric(dt2_ef$dose_cum>=10)+
    as.numeric(dt2_ef$dose_cum>=15)+
    as.numeric(dt2_ef$dose_cum>=25)+
    as.numeric(dt2_ef$dose_cum>=50)+
    as.numeric(dt2_ef$dose_cum>=75)+
    as.numeric(dt2_ef$dose_cum>=100)
  
  dt2_ef$cumcat2_short <- 1+
    as.numeric(dt2_ef$dose_cum>=5)+
    as.numeric(dt2_ef$dose_cum>=10)+
    as.numeric(dt2_ef$dose_cum>=15)+
    as.numeric(dt2_ef$dose_cum>=25)+
    as.numeric(dt2_ef$dose_cum>=50)
  
  # categorical exposure MH
  dt2_ef$cumcat2_mh <- 1+
    as.numeric(dt2_ef$dose_cum>=5)+
    as.numeric(dt2_ef$dose_cum>=10)+
    as.numeric(dt2_ef$dose_cum>=12)+
    as.numeric(dt2_ef$dose_cum>=15)+
    as.numeric(dt2_ef$dose_cum>=20)+
    as.numeric(dt2_ef$dose_cum>=25)+
    as.numeric(dt2_ef$dose_cum>=35)
  
  #### last outcome cleaning ####
  names(dt2_ef)
  #dt2_ef <- dt2_ef[,c(1:43,58,44:57,59,60:66)]
  #dt2_ef <- dt2_ef[,c(1:23,43:46,48,24:25,47,26:42,49:63)]
  
  # set 0 to the outcome columns at the times of no-diagnostic
  for(i in 32:47)
    dt2_ef[which(dt2_ef$n_pe!=0),i] <- 0
  
  #### save to a file ####
  
  saveRDS(dt2_ef,paste0("transformed_cohorts/", namesdf[l]))
}
