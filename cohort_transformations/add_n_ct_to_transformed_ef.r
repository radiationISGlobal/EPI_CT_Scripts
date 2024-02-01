# Author:          Francesc Badia
# Date:            2018-07-05 09:14:54
# --------------
# Description:     add number of ct variable: same as n_pe but change the last event to max(n_ct) by patientids       
# --------------

library(dplyr)

rm(list=ls())
setwd('C:/fbadia/EPICT/epict_analysis/20190604/')

dt <- readRDS("transformed_cohorts/dt1_ef.rds")
lag <- 2

# summary of number of cts: counting repetitions
a <- plyr::count(dt$n_pe)
# summary of number of cts
b <- dt %>%
  group_by(patientids) %>%
  summarize(n_ct=max(n_pe)) %>%
  count(n_ct)

#dt1 <- dt[dt$exit_age - dt$age >= lag, c("patientids","n_pe")]
dt1 <- dt[, c("patientids","n_pe")]
dt1 <- dt1 %>% group_by(patientids) %>% mutate(n_ct=max(n_pe))
dt1 <- dt1[dt1$n_pe == 0,]

dt <- left_join(dt,dt1,by=c("patientids","n_pe"))
dt$n_ct[which(dt$n_pe!=0)] <- dt$n_pe[which(dt$n_pe!=0)]
dt$n_ct[which(dt$n_pe==-1)] <- 0

dt$n_ct_cat <- 1 + as.numeric(dt$n_ct>=2) + as.numeric(dt$n_ct>=4) + as.numeric(dt$n_ct>=6)
dt$n_ct_cat[which(dt$n_ct_cat==1)] <- "1_ct"
dt$n_ct_cat[which(dt$n_ct_cat==2)] <- "2_3_ct"
dt$n_ct_cat[which(dt$n_ct_cat==3)] <- "4_5_ct"
dt$n_ct_cat[which(dt$n_ct_cat==4)] <- "6_plus_ct"

# View(dt[,c("patientids","n_pe","n_ct")])
saveRDS(dt,"transformed_cohorts/dt1_ef_n_ct.rds")
