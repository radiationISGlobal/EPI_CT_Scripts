# Author:        Francesc Badia
# Date:         2018-01-23 10:34:25
# ------------------------------
# Description:  linERR to all cohort Myeloid with grouped doses 0-5 / 5-10 / 10+ years before case occurs
# ------------------------------

rm(list=ls())
setwd("Y:/EPI CT Analysis/2022_new_grouping")
source('rERR_ModifiedFunctions/rERR_jf.r')
library(dplyr)

# function to save the fit results
source("analysis_8/scripts/f_results_fit_8.r")
#source("analysis_8/scripts/f_results_fit_8_jf.r")

dt1 <- readRDS("transformed_cohorts/dt1_new_outcomes.rds")
lag <- 2

output_path     <- "analysis_8/results/"
output_filename <- "ec_res_Myeloid_ef_gr_dose_backw_all.csv"
outcome_name    <- "Myeloid"
data_set_format <- "event_fromat"

# formula1: to compute the first risksets, and allow us to re - compute the doses until case occurs
formula1 <- Surv(entry_age,exit_age,Myeloid)~lin(dose_cum)+strata(sex,country,birthcohort)
# formula2: once cdose_0_4 cdose_5_9 cdose_10_ computed, fit the model
formula2 <- Surv(entry_age,exit_age,Myeloid)~lin(cdose_0_4,cdose_5_9,cdose_10_)+strata(sex,country,birthcohort)

#f_fit_linERR_all
id_name   <- "patientids"
time_name <- "age"
lag       <- 2

# first model data transformation
dt2 <- f_to_model_data(formula1, data=dt1, id_name="patientids", time_name="age")

n_lin_vars    <- attr(dt2, "n_lin_vars")
n_loglin_vars <- attr(dt2, "n_loglin_vars")

# pre risk sets
rsets <- f_risksets(formula1, data = dt2, lag, id_name, time_name)

# final new data.frame
dt3 <-data.frame()
last_row_prev_rset <- 0
rsets_2 <- list()
# iteration over cases / rsets
for(i in 1:length(rsets))
{
  # final data set shape
  #c("n_row","patientids","n_pe","entry_age","exit_age","Myeloid","age","dose_cum","sex","country","birthcohort")
  # risk set i information
  row_id_case <- names(rsets)[i]
  id_case     <- dt2$patientids[as.numeric(row_id_case)]
  ft          <- unique(dt2$exit_age[which(dt2$patientids==id_case)])
  ids         <- dt1$patientids[rsets[[i]]]
  
  # take the rows or colums of interest
  dt <- dt1[dt1$patientids %in% ids,]
  dt <- select(dt,patientids,entry_age,exit_age,Myeloid,age,ActMar_mean,n_pe,sex,country,birthcohort)
  
  # cut by the lag: take only exposures until ft-lag
  lag <- 2
  dt  <- dt[which(dt$age < ft-lag),]
  
  # cumulative per period: filter
  dt_0_4 <- dt[which(dt$age >= ft-5),]
  dt_5_9 <- dt[which(dt$age <  ft-5  & dt$age >= ft-10),]
  dt_10_ <- dt[which(dt$age <  ft-10),]
  # cumulative per period: sum
  dt_0_4 <- dt_0_4 %>% group_by(patientids) %>% summarize(cdose_0_4=sum(ActMar_mean))
  dt_5_9 <- dt_5_9 %>% group_by(patientids) %>% summarize(cdose_5_9=sum(ActMar_mean))  
  dt_10_ <- dt_10_ %>% group_by(patientids) %>% summarize(cdose_10_=sum(ActMar_mean))  
  
  # put together
  aux <- full_join(dt_0_4,dt_5_9,by="patientids")
  aux <- full_join(aux,dt_10_,by="patientids")
  aux[is.na(aux)] <- 0
  
  # create the new data set with the shape for the model
  dt <- dt %>% select(patientids,entry_age,exit_age,Myeloid,sex,country,birthcohort)
  dt <- distinct(dt)
  aux <- left_join(aux,dt,by="patientids")
  
  # for the further fitting, don't care the n_pe, only not 0 for not ending a case and 0 for a exit event of a case
  aux$n_row <- 1:dim(aux)[1] + last_row_prev_rset
  aux$n_pe  <- 1
  aux$age   <- 0 
  
  aux <- aux[,c("n_row","patientids","n_pe","entry_age","exit_age","Myeloid","age","cdose_0_4","cdose_5_9","cdose_10_","sex","country","birthcohort")]
  
  # add the row with the information of the case
  case_row <- which(aux$patientids==id_case)
  row_case <- aux[case_row,]
  row_case$n_pe<-0
  row_case$Myeloid <- 1
  row_case$n_row <- max(aux$n_row)+1
  
  # new risksets
  rsets_2[[i]] <- aux$n_row
  
  aux <- rbind(aux,row_case)
  dt3 <- bind_rows(dt3,aux)
  names(rsets_2)[i] <- case_row + last_row_prev_rset
  last_row_prev_rset <- dim(dt3)[1]
}

cases <- unique(dt3[dt3$n_pe==0 & dt3$Myeloid==1, c("patientids", "n_pe","cdose_0_4", "cdose_5_9", "cdose_10_")])
n1 <- nrow(cases[cases$cdose_0_4>0,])
n2 <- nrow(cases[cases$cdose_5_9>0,])
n3 <- nrow(cases[cases$cdose_10_>0,])

# fit the model
fit <- f_fit_linERR(formula2, data = dt3, rsets=rsets_2, n_lin_vars=3,n_loglin_vars=0, id_name="patientids", time_name="age")

# save results
results <- f_results_fit_8(fit, c(n1,n2,n3))

write.table(results,paste0(output_path,output_filename),row.names = F)


3645.307323 - 3651.92779738679 = 6.620474

1 - pchisq(6.620474, 2, lower.tail = T)

