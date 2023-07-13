## Tables from "TABLES EPI-CT International haematological malignancies risk analysis_2022_08_24"

## TABLE 1

library(dplyr)
library(readxl) 
library(tidyr)
library(dplyr)
library(writexl)


rm(list=ls()) 

setwd("Y:/EPI CT Analysis/2022_new_grouping")

dt1 <- readRDS("transformed_cohorts/dt1_new_outcomes_v2.rds")
names(dt1)

age1st_cat <- vector()
age1st_cat[which(dt1$age1st < 1)] <- "1"
age1st_cat[which(1 <= dt1$age1st & dt1$age1st < 5)] <- "2"
age1st_cat[which(5 <= dt1$age1st & dt1$age1st < 10)] <- "3"
age1st_cat[which(10 <= dt1$age1st & dt1$age1st < 15)] <- "4"
age1st_cat[which(15 <= dt1$age1st)] <- "5"

dt1 <- cbind(dt1,age1st_cat)
colnames(dt1)[70] <- "age1st_cat"

yy_since1st <- (dt1$exit - dt1$ct1st)/365.25
dt1 <- cbind(dt1,yy_since1st)
colnames(dt1)[71] <- "yy_since1st"

yy_since1st_cat <- vector()
yy_since1st_cat[which(2 <= dt1$yy_since1st & dt1$yy_since1st < 5)] <- 1
yy_since1st_cat[which(5 <= dt1$yy_since1st & dt1$yy_since1st < 10)] <- 2
yy_since1st_cat[which(10 <= dt1$yy_since1st & dt1$yy_since1st < 15)] <- 3
yy_since1st_cat[which(15 <= dt1$yy_since1st)] <- 4

dt1 <- cbind(dt1, yy_since1st_cat)
colnames(dt1)[72] <- "yy_since1st_cat"

agecat <- vector()
agecat[which(2 <= dt1$age & dt1$age < 20)] <- 1
agecat[which(20 <= dt1$age & dt1$age < 30)] <- 2
agecat[which(30 <= dt1$age & dt1$age < 40)] <- 3
agecat[which(40 <= dt1$age)] <- 4

dt1 <- cbind(dt1, agecat)
colnames(dt1)[73] <- "agecat"


dt1_uniq <- dt1[which(!duplicated(dt1$patientids)),]
dt1_uniq$py <- dt1_uniq$exit_age - dt1_uniq$entry_age

dt1_an <- dt1[which(dt1$n_pe != "-1"),]
dt1_an <- dt1_an[which(dt1_an$dose_num != 0),]
dt1_an_cases_patients <- dt1 %>% group_by(patientids) %>% filter(any(all_excl_therap_syndrel == 1))
dt1_an_ind <- dt1_an %>% group_by(patientids) %>% filter(n_pe == max(n_pe))
dt1_an_ind_cases <- dt1_an_ind[which(dt1_an_ind$patientids %in% dt1_an_cases_patients$patientids),]

mat <- matrix(nrow = 39, ncol = 1)

#overall 
mat_overall <- matrix(nrow = 1, ncol = 19)


mat_overall[1,1] <- table(dt1$all_excl_therap_syndrel == 1)[2]
mat_overall[1,2] <- 100

mat_overall[1,3] <- table(dt1$Lymphoid == 1)[2]
mat_overall[1,4] <- table(dt1$HL == 1)[2]
mat_overall[1,5] <- table(dt1$NHL == 1)[2]
mat_overall[1,6] <- table(dt1$NHL_Bcell == 1)[2]
mat_overall[1,7] <- table(dt1$NHL_Tcell == 1)[2]
mat_overall[1,8] <- table(dt1$NHL_Precursor_cell == 1)[2]

mat_overall[1,9] <- table(dt1$Myeloid == 1)[2]
mat_overall[1,10] <- table(dt1$AML_prec_ALMP_ALAL == 1)[2]
mat_overall[1,11] <- table(dt1$MPN_MDSMPN_MDS == 1)[2]
mat_overall[1,12] <- table(dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]
mat_overall[1,13] <- table(dt1$MPN == 1)[2]
mat_overall[1,14] <- table(dt1$HISTIOCYTIC_DENDRITIC == 1)[2]
mat_overall[1,15] <- table(dt1$UNSP == 1)[2]

mat_overall[1,16] <- length(unique(dt1$patientids))
mat_overall[1,17] <- length(unique(dt1$patientids))/length(unique(dt1$patientids))*100

mat_overall[1,18] <- sum(dt1_uniq$py)
mat_overall[1,19] <- sum(dt1_uniq$py)/sum(dt1_uniq$py)*100

row.names(mat_overall)[1] <- "Overall"

#Sex
mat_sex <- matrix(nrow = 3, ncol = 19)

row.names(mat_sex) <- c("Sex", "Male", "Female")

mat_sex[2,1] <- table(dt1$sex == 1 & dt1$all_excl_therap_syndrel == 1)[2]
mat_sex[2,2] <- round((mat_sex[2,1]/mat_overall[1,1])*100,1)

mat_sex[3,1] <- table(dt1$sex == 2 & dt1$all_excl_therap_syndrel == 1)[2]
mat_sex[3,2] <- round((mat_sex[3,1]/mat_overall[1,1])*100,1)
#male
mat_sex[2,3] <- table(dt1$sex == 1 & dt1$Lymphoid == 1)[2] 
mat_sex[2,4] <- table(dt1$sex == 1 & dt1$HL == 1)[2] 
mat_sex[2,5] <- table(dt1$sex == 1 & dt1$NHL == 1)[2] 
mat_sex[2,6] <- table(dt1$sex == 1 & dt1$NHL_Bcell == 1)[2] 
mat_sex[2,7] <- table(dt1$sex == 1 & dt1$NHL_Tcell == 1)[2] 
mat_sex[2,8] <- table(dt1$sex == 1 & dt1$NHL_Precursor_cell == 1)[2] 

mat_sex[2,9] <- table(dt1$sex == 1 & dt1$Myeloid == 1)[2] 

mat_sex[2,10] <- table(dt1$sex == 1 & dt1$AML_prec_ALMP_ALAL == 1)[2] 
mat_sex[2,11] <- table(dt1$sex == 1 & dt1$MPN_MDSMPN_MDS == 1)[2] 
mat_sex[2,12] <- table(dt1$sex == 1 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]
mat_sex[2,13] <- table(dt1$sex == 1 & dt1$MPN == 1)[2]
mat_sex[2,14] <- table(dt1$sex == 1 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2] 
mat_sex[2,15] <- table(dt1$sex == 1 & dt1$UNSP == 1)[2] 

mat_sex[2,16] <- length(dt1_uniq$patientids[which(dt1_uniq$sex == 1)])
mat_sex[2,17] <- length(dt1_uniq$patientids[which(dt1_uniq$sex == 1)])/length(unique(dt1$patientids))*100

mat_sex[2,18] <- sum(dt1_uniq$py[which(dt1_uniq$sex == 1)])
mat_sex[2,19] <- sum(dt1_uniq$py[which(dt1_uniq$sex == 1)])/sum(dt1_uniq$py)*100

#female
mat_sex[3,3] <- table(dt1$sex == 2 & dt1$Lymphoid == 1)[2] 
mat_sex[3,4] <- table(dt1$sex == 2 & dt1$HL == 1)[2] 
mat_sex[3,5] <- table(dt1$sex == 2 & dt1$NHL == 1)[2] 
mat_sex[3,6] <- table(dt1$sex == 2 & dt1$NHL_Bcell == 1)[2] 
mat_sex[3,7] <- table(dt1$sex == 2 & dt1$NHL_Tcell == 1)[2] 
mat_sex[3,8] <- table(dt1$sex == 2 & dt1$NHL_Precursor_cell == 1)[2] 

mat_sex[3,9] <- table(dt1$sex == 2 & dt1$Myeloid == 1)[2] 
mat_sex[3,10] <- table(dt1$sex == 2 & dt1$AML_prec_ALMP_ALAL == 1)[2] 
mat_sex[3,11] <- table(dt1$sex == 2 & dt1$MPN_MDSMPN_MDS == 1)[2] 
mat_sex[3,12] <- table(dt1$sex == 2 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2] 
mat_sex[3,13] <- table(dt1$sex == 2 & dt1$MPN == 1)[2] 
mat_sex[3,14] <- table(dt1$sex == 2 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2] 
mat_sex[3,15] <- table(dt1$sex == 2 & dt1$UNSP == 1)[2] 

mat_sex[3,16] <- length(dt1_uniq$patientids[which(dt1_uniq$sex == 2)])
mat_sex[3,17] <- length(dt1_uniq$patientids[which(dt1_uniq$sex == 2)])/length(unique(dt1$patientids))*100

mat_sex[3,18] <- sum(dt1_uniq$py[which(dt1_uniq$sex == 2)])
mat_sex[3,19] <- sum(dt1_uniq$py[which(dt1_uniq$sex == 2)])/sum(dt1_uniq$py)*100

#Age cats


mat_agecat <- matrix(nrow = 6, ncol = 19)
row.names(mat_agecat) <- c("Age at first CT", "<1","1-<5","5-<10","10-<15",">15")
#<1
mat_agecat[2,1] <- table(dt1$age1st_cat == 1 & dt1$all_excl_therap_syndrel == 1)[2]
mat_agecat[2,2] <- round((mat_agecat[2,1]/mat_overall[1,1])*100,1)
mat_agecat[2,3] <- table(dt1$age1st_cat == 1 & dt1$Lymphoid == 1)[2] 
mat_agecat[2,4] <- table(dt1$age1st_cat == 1 & dt1$HL == 1)[2] 
mat_agecat[2,5] <- table(dt1$age1st_cat == 1 & dt1$NHL == 1)[2] 
mat_agecat[2,6] <- table(dt1$age1st_cat == 1 & dt1$NHL_Bcell == 1)[2] 
mat_agecat[2,7] <- table(dt1$age1st_cat == 1 & dt1$NHL_Tcell == 1)[2] 
mat_agecat[2,8] <- table(dt1$age1st_cat == 1 & dt1$NHL_Precursor_cell == 1)[2] 

mat_agecat[2,9] <- table(dt1$age1st_cat == 1 & dt1$Myeloid == 1)[2] 
mat_agecat[2,10] <- table(dt1$age1st_cat == 1 & dt1$AML_prec_ALMP_ALAL == 1)[2] 
mat_agecat[2,11] <- table(dt1$age1st_cat == 1 & dt1$MPN_MDSMPN_MDS == 1)[2] 
mat_agecat[2,12] <- table(dt1$age1st_cat == 1 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2] 
mat_agecat[2,13] <- table(dt1$age1st_cat == 1 & dt1$MPN == 1)[2] 
mat_agecat[2,14] <- table(dt1$age1st_cat == 1 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2] 
mat_agecat[2,15] <- table(dt1$age1st_cat == 1 & dt1$UNSP == 1)[2] 

mat_agecat[2,16] <- length(dt1_uniq$patientids[which(dt1_uniq$age1st_cat == 1)])
mat_agecat[2,17] <- length(dt1_uniq$patientids[which(dt1_uniq$age1st_cat == 1)])/length(unique(dt1$patientids))*100

mat_agecat[2,18] <- sum(dt1_uniq$py[which(dt1_uniq$age1st_cat == 1)])
mat_agecat[2,19] <- sum(dt1_uniq$py[which(dt1_uniq$age1st_cat == 1)])/sum(dt1_uniq$py)*100


#1-<5
mat_agecat[3,1] <- table(dt1$age1st_cat == 2 & dt1$all_excl_therap_syndrel == 1)[2]
mat_agecat[3,2] <- round((mat_agecat[3,1]/mat_overall[1,1])*100,1)
mat_agecat[3,3] <- table(dt1$age1st_cat == 2 & dt1$Lymphoid == 1)[2] 
mat_agecat[3,4] <- table(dt1$age1st_cat == 2 & dt1$HL == 1)[2] 
mat_agecat[3,5] <- table(dt1$age1st_cat == 2 & dt1$NHL == 1)[2] 
mat_agecat[3,6] <- table(dt1$age1st_cat == 2 & dt1$NHL_Bcell == 1)[2] 
mat_agecat[3,7] <- table(dt1$age1st_cat == 2 & dt1$NHL_Tcell == 1)[2] 
mat_agecat[3,8] <- table(dt1$age1st_cat == 2 & dt1$NHL_Precursor_cell == 1)[2] 

mat_agecat[3,9] <- table(dt1$age1st_cat == 2 & dt1$Myeloid == 1)[2] 
mat_agecat[3,10] <- table(dt1$age1st_cat == 2 & dt1$AML_prec_ALMP_ALAL == 1)[2] 
mat_agecat[3,11] <- table(dt1$age1st_cat == 2 & dt1$MPN_MDSMPN_MDS == 1)[2] 
mat_agecat[3,12] <- table(dt1$age1st_cat == 2 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2] 
mat_agecat[3,13] <- table(dt1$age1st_cat == 2 & dt1$MPN == 1)[2] 
mat_agecat[3,14] <- table(dt1$age1st_cat == 2 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2] 
mat_agecat[3,15] <- table(dt1$age1st_cat == 2 & dt1$UNSP == 1)[2] 

mat_agecat[3,16] <- length(dt1_uniq$patientids[which(dt1_uniq$age1st_cat == 2)])
mat_agecat[3,17] <- length(dt1_uniq$patientids[which(dt1_uniq$age1st_cat == 2)])/length(unique(dt1$patientids))*100

mat_agecat[3,18] <- sum(dt1_uniq$py[which(dt1_uniq$age1st_cat == 2)])
mat_agecat[3,19] <- sum(dt1_uniq$py[which(dt1_uniq$age1st_cat == 2)])/sum(dt1_uniq$py)*100

#5-<10
mat_agecat[4,1] <- table(dt1$age1st_cat == 3 & dt1$all_excl_therap_syndrel == 1)[2] 
mat_agecat[4,2] <- round((mat_agecat[4,1]/mat_overall[1,1])*100,1)
mat_agecat[4,3] <- table(dt1$age1st_cat == 3 & dt1$Lymphoid == 1)[2]  
mat_agecat[4,4] <- table(dt1$age1st_cat == 3 & dt1$HL == 1)[2]  
mat_agecat[4,5] <- table(dt1$age1st_cat == 3 & dt1$NHL == 1)[2]  
mat_agecat[4,6] <- table(dt1$age1st_cat == 3 & dt1$NHL_Bcell == 1)[2]  
mat_agecat[4,7] <- table(dt1$age1st_cat == 3 & dt1$NHL_Tcell == 1)[2]  
mat_agecat[4,8] <- table(dt1$age1st_cat == 3 & dt1$NHL_Precursor_cell == 1)[2]  

mat_agecat[4,9] <- table(dt1$age1st_cat == 3 & dt1$Myeloid == 1)[2]  
mat_agecat[4,10] <- table(dt1$age1st_cat == 3 & dt1$AML_prec_ALMP_ALAL == 1)[2]  
mat_agecat[4,11] <- table(dt1$age1st_cat == 3 & dt1$MPN_MDSMPN_MDS == 1)[2]  
mat_agecat[4,12] <- table(dt1$age1st_cat == 3 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]  
mat_agecat[4,13] <- table(dt1$age1st_cat == 3 & dt1$MPN == 1)[2]  
mat_agecat[4,14] <- table(dt1$age1st_cat == 3 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]  
mat_agecat[4,15] <- table(dt1$age1st_cat == 3 & dt1$UNSP == 1)[2]  

mat_agecat[4,16] <- length(dt1_uniq$patientids[which(dt1_uniq$age1st_cat == 3)])
mat_agecat[4,17] <- length(dt1_uniq$patientids[which(dt1_uniq$age1st_cat == 3)])/length(unique(dt1$patientids))*100

mat_agecat[4,18] <- sum(dt1_uniq$py[which(dt1_uniq$age1st_cat == 3)])
mat_agecat[4,19] <- sum(dt1_uniq$py[which(dt1_uniq$age1st_cat == 3)])/sum(dt1_uniq$py)*100

#10-<15
mat_agecat[5,1] <- table(dt1$age1st_cat == 4 & dt1$all_excl_therap_syndrel == 1)[2]  
mat_agecat[5,2] <- round((mat_agecat[5,1]/mat_overall[1,1])*100,1)
mat_agecat[5,3] <- table(dt1$age1st_cat == 4 & dt1$Lymphoid == 1)[2]   
mat_agecat[5,4] <- table(dt1$age1st_cat == 4 & dt1$HL == 1)[2]   
mat_agecat[5,5] <- table(dt1$age1st_cat == 4 & dt1$NHL == 1)[2]   
mat_agecat[5,6] <- table(dt1$age1st_cat == 4 & dt1$NHL_Bcell == 1)[2]   
mat_agecat[5,7] <- table(dt1$age1st_cat == 4 & dt1$NHL_Tcell == 1)[2]   
mat_agecat[5,8] <- table(dt1$age1st_cat == 4 & dt1$NHL_Precursor_cell == 1)[2]   

mat_agecat[5,9] <- table(dt1$age1st_cat == 4 & dt1$Myeloid == 1)[2]   
mat_agecat[5,10] <- table(dt1$age1st_cat == 4 & dt1$AML_prec_ALMP_ALAL == 1)[2]   
mat_agecat[5,11] <- table(dt1$age1st_cat == 4 & dt1$MPN_MDSMPN_MDS == 1)[2]   
mat_agecat[5,12] <- table(dt1$age1st_cat == 4 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]   
mat_agecat[5,13] <- table(dt1$age1st_cat == 4 & dt1$MPN == 1)[2]   
mat_agecat[5,14] <- table(dt1$age1st_cat == 4 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]   
mat_agecat[5,15] <- table(dt1$age1st_cat == 4 & dt1$UNSP == 1)[2]   

mat_agecat[5,16] <- length(dt1_uniq$patientids[which(dt1_uniq$age1st_cat == 4)])
mat_agecat[5,17] <- length(dt1_uniq$patientids[which(dt1_uniq$age1st_cat == 4)])/length(unique(dt1$patientids))*100

mat_agecat[5,18] <- sum(dt1_uniq$py[which(dt1_uniq$age1st_cat == 4)])
mat_agecat[5,19] <- sum(dt1_uniq$py[which(dt1_uniq$age1st_cat == 4)])/sum(dt1_uniq$py)*100

#???15
mat_agecat[6,1] <- table(dt1$age1st_cat == 5 & dt1$all_excl_therap_syndrel == 1)[2]   
mat_agecat[6,2] <- round((mat_agecat[6,1]/mat_overall[1,1])*100,1)
mat_agecat[6,3] <- table(dt1$age1st_cat == 5 & dt1$Lymphoid == 1)[2]    
mat_agecat[6,4] <- table(dt1$age1st_cat == 5 & dt1$HL == 1)[2]    
mat_agecat[6,5] <- table(dt1$age1st_cat == 5 & dt1$NHL == 1)[2]    
mat_agecat[6,6] <- table(dt1$age1st_cat == 5 & dt1$NHL_Bcell == 1)[2]    
mat_agecat[6,7] <- table(dt1$age1st_cat == 5 & dt1$NHL_Tcell == 1)[2]    
mat_agecat[6,8] <- table(dt1$age1st_cat == 5 & dt1$NHL_Precursor_cell == 1)[2]    

mat_agecat[6,9] <- table(dt1$age1st_cat == 5 & dt1$Myeloid == 1)[2]    
mat_agecat[6,10] <- table(dt1$age1st_cat == 5 & dt1$AML_prec_ALMP_ALAL == 1)[2]    
mat_agecat[6,11] <- table(dt1$age1st_cat == 5 & dt1$MPN_MDSMPN_MDS == 1)[2]    
mat_agecat[6,12] <- table(dt1$age1st_cat == 5 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]    
mat_agecat[6,13] <- table(dt1$age1st_cat == 5 & dt1$MPN == 1)[2]    
mat_agecat[6,14] <- table(dt1$age1st_cat == 5 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]    
mat_agecat[6,15] <- table(dt1$age1st_cat == 5 & dt1$UNSP == 1)[2]    

mat_agecat[6,16] <- length(dt1_uniq$patientids[which(dt1_uniq$age1st_cat == 5)])
mat_agecat[6,17] <- length(dt1_uniq$patientids[which(dt1_uniq$age1st_cat == 5)])/length(unique(dt1$patientids))*100

mat_agecat[6,18] <- sum(dt1_uniq$py[which(dt1_uniq$age1st_cat == 5)])
mat_agecat[6,19] <- sum(dt1_uniq$py[which(dt1_uniq$age1st_cat == 5)])/sum(dt1_uniq$py)*100

# Years since first CT
names(dt1)


mat_yys1ct <- matrix(nrow = 5, ncol = 15)
rownames(mat_yys1ct) <- c("Years since first CT","2-<5","5-<10","10-<15",">15")

#2-<5
mat_yys1ct[2,1] <- table(dt1$yy_since1st_cat == 1 & dt1$all_excl_therap_syndrel == 1)[2]
mat_yys1ct[2,2] <- round((mat_yys1ct[2,1]/mat_overall[1,1])*100,1)
mat_yys1ct[2,3] <- table(dt1$yy_since1st_cat == 1 &  dt1$Lymphoid == 1)[2] 
mat_yys1ct[2,4] <- table(dt1$yy_since1st_cat == 1 &  dt1$HL == 1)[2] 
mat_yys1ct[2,5] <- table(dt1$yy_since1st_cat == 1 &  dt1$NHL == 1)[2] 
mat_yys1ct[2,6] <- table(dt1$yy_since1st_cat == 1 &  dt1$NHL_Bcell == 1)[2] 
mat_yys1ct[2,7] <- table(dt1$yy_since1st_cat == 1 &  dt1$NHL_Tcell == 1)[2] 
mat_yys1ct[2,8] <- table(dt1$yy_since1st_cat == 1 &  dt1$NHL_Precursor_cell == 1)[2] 

mat_yys1ct[2,9] <- table(dt1$yy_since1st_cat == 1 &  dt1$Myeloid == 1)[2] 
mat_yys1ct[2,10] <- table(dt1$yy_since1st_cat == 1 &  dt1$AML_prec_ALMP_ALAL == 1)[2] 
mat_yys1ct[2,11] <- table(dt1$yy_since1st_cat == 1 &  dt1$MPN_MDSMPN_MDS == 1)[2] 
mat_yys1ct[2,12] <- table(dt1$yy_since1st_cat == 1 &  dt1$AML_AL_ALMP_AL_excl_gen == 1)[2] 
mat_yys1ct[2,13] <- table(dt1$yy_since1st_cat == 1 &  dt1$MPN == 1)[2] 
mat_yys1ct[2,14] <- table(dt1$yy_since1st_cat == 1 &  dt1$HISTIOCYTIC_DENDRITIC == 1)[2] 
mat_yys1ct[2,15] <- table(dt1$yy_since1st_cat == 1 &  dt1$UNSP == 1)[2] 

mat_yys1ct[2,16] <- length(dt1_uniq$patientids[which(dt1_uniq$yy_since1st_cat == 1)])
mat_yys1ct[2,17] <- length(dt1_uniq$patientids[which(dt1_uniq$yy_since1st_cat == 1)])/length(unique(dt1$patientids))*100

mat_yys1ct[2,18] <- sum(dt1_uniq$py[which(dt1_uniq$yy_since1st_cat == 1)])
mat_yys1ct[2,19] <- sum(dt1_uniq$py[which(dt1_uniq$yy_since1st_cat == 1)])/sum(dt1_uniq$py)*100

#5-<10
mat_yys1ct[3,1] <- table(dt1$yy_since1st_cat == 2 & dt1$all_excl_therap_syndrel == 1)[2] 
mat_yys1ct[3,2] <- round((mat_yys1ct[3,1]/mat_overall[1,1])*100,1)
mat_yys1ct[3,3] <- table(dt1$yy_since1st_cat == 2 & dt1$Lymphoid == 1)[2]  
mat_yys1ct[3,4] <- table(dt1$yy_since1st_cat == 2 & dt1$HL == 1)[2]  
mat_yys1ct[3,5] <- table(dt1$yy_since1st_cat == 2 & dt1$NHL == 1)[2]  
mat_yys1ct[3,6] <- table(dt1$yy_since1st_cat == 2 & dt1$NHL_Bcell == 1)[2]  
mat_yys1ct[3,7] <- table(dt1$yy_since1st_cat == 2 & dt1$NHL_Tcell == 1)[2]  
mat_yys1ct[3,8] <- table(dt1$yy_since1st_cat == 2 & dt1$NHL_Precursor_cell == 1)[2]  

mat_yys1ct[3,9] <- table(dt1$yy_since1st_cat == 2 & dt1$Myeloid == 1)[2]  
mat_yys1ct[3,10] <- table(dt1$yy_since1st_cat == 2 & dt1$AML_prec_ALMP_ALAL == 1)[2]  
mat_yys1ct[3,11] <- table(dt1$yy_since1st_cat == 2 & dt1$MPN_MDSMPN_MDS == 1)[2]  
mat_yys1ct[3,12] <- table(dt1$yy_since1st_cat == 2 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]  
mat_yys1ct[3,13] <- table(dt1$yy_since1st_cat == 2 & dt1$MPN == 1)[2]  
mat_yys1ct[3,14] <- table(dt1$yy_since1st_cat == 2 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]  
mat_yys1ct[3,15] <- table(dt1$yy_since1st_cat == 2 & dt1$UNSP == 1)[2]  

mat_yys1ct[3,16] <- length(dt1_uniq$patientids[which(dt1_uniq$yy_since1st_cat == 2)])
mat_yys1ct[3,17] <- length(dt1_uniq$patientids[which(dt1_uniq$yy_since1st_cat == 2)])/length(unique(dt1$patientids))*100

mat_yys1ct[3,18] <- sum(dt1_uniq$py[which(dt1_uniq$yy_since1st_cat == 2)])
mat_yys1ct[3,19] <- sum(dt1_uniq$py[which(dt1_uniq$yy_since1st_cat == 2)])/sum(dt1_uniq$py)*100



#10-<15
mat_yys1ct[4,1] <- table(dt1$yy_since1st_cat == 3 & dt1$all_excl_therap_syndrel == 1)[2] 
mat_yys1ct[4,2] <- round((mat_yys1ct[4,1]/mat_overall[1,1])*100,1)
mat_yys1ct[4,3] <- table(dt1$yy_since1st_cat == 3 & dt1$Lymphoid == 1)[2]  
mat_yys1ct[4,4] <- table(dt1$yy_since1st_cat == 3 & dt1$HL == 1)[2]  
mat_yys1ct[4,5] <- table(dt1$yy_since1st_cat == 3 & dt1$NHL == 1)[2]  
mat_yys1ct[4,6] <- table(dt1$yy_since1st_cat == 3 & dt1$NHL_Bcell == 1)[2]  
mat_yys1ct[4,7] <- table(dt1$yy_since1st_cat == 3 & dt1$NHL_Tcell == 1)[2]  
mat_yys1ct[4,8] <- table(dt1$yy_since1st_cat == 3 & dt1$NHL_Precursor_cell == 1)[2]  

mat_yys1ct[4,9] <- table(dt1$yy_since1st_cat == 3 & dt1$Myeloid == 1)[2]  
mat_yys1ct[4,10] <- table(dt1$yy_since1st_cat == 3 & dt1$AML_prec_ALMP_ALAL == 1)[2]  
mat_yys1ct[4,11] <- table(dt1$yy_since1st_cat == 3 & dt1$MPN_MDSMPN_MDS == 1)[2]  
mat_yys1ct[4,12] <- table(dt1$yy_since1st_cat == 3 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]  
mat_yys1ct[4,13] <- table(dt1$yy_since1st_cat == 3 & dt1$MPN == 1)[2]  
mat_yys1ct[4,14] <- table(dt1$yy_since1st_cat == 3 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]  
mat_yys1ct[4,15] <- table(dt1$yy_since1st_cat == 3 & dt1$UNSP == 1)[2]  

mat_yys1ct[4,16] <- length(dt1_uniq$patientids[which(dt1_uniq$yy_since1st_cat == 3)])
mat_yys1ct[4,17] <- length(dt1_uniq$patientids[which(dt1_uniq$yy_since1st_cat == 3)])/length(unique(dt1$patientids))*100

mat_yys1ct[4,18] <- sum(dt1_uniq$py[which(dt1_uniq$yy_since1st_cat == 3)])
mat_yys1ct[4,19] <- sum(dt1_uniq$py[which(dt1_uniq$yy_since1st_cat == 3)])/sum(dt1_uniq$py)*100

#???15
mat_yys1ct[5,1] <- table(dt1$yy_since1st_cat == 4 & dt1$all_excl_therap_syndrel == 1)[2] 
mat_yys1ct[5,2] <- round((mat_yys1ct[5,1]/mat_overall[1,1])*100,1)
mat_yys1ct[5,3] <- table(dt1$yy_since1st_cat == 4 & dt1$Lymphoid == 1)[2]  
mat_yys1ct[5,4] <- table(dt1$yy_since1st_cat == 4 & dt1$HL == 1)[2]  
mat_yys1ct[5,5] <- table(dt1$yy_since1st_cat == 4 & dt1$NHL == 1)[2]  
mat_yys1ct[5,6] <- table(dt1$yy_since1st_cat == 4 & dt1$NHL_Bcell == 1)[2]  
mat_yys1ct[5,7] <- table(dt1$yy_since1st_cat == 4 & dt1$NHL_Tcell == 1)[2]  
mat_yys1ct[5,8] <- table(dt1$yy_since1st_cat == 4 & dt1$NHL_Precursor_cell == 1)[2]  

mat_yys1ct[5,9] <- table(dt1$yy_since1st_cat == 4 & dt1$Myeloid == 1)[2]  
mat_yys1ct[5,10] <- table(dt1$yy_since1st_cat == 4 & dt1$AML_prec_ALMP_ALAL == 1)[2]  
mat_yys1ct[5,11] <- table(dt1$yy_since1st_cat == 4 & dt1$MPN_MDSMPN_MDS == 1)[2]  
mat_yys1ct[5,12] <- table(dt1$yy_since1st_cat == 4 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]  
mat_yys1ct[5,13] <- table(dt1$yy_since1st_cat == 4 & dt1$MPN == 1)[2]  
mat_yys1ct[5,14] <- table(dt1$yy_since1st_cat == 4 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]  
mat_yys1ct[5,15] <- table(dt1$yy_since1st_cat == 4 & dt1$UNSP == 1)[2]  

mat_yys1ct[5,16] <- length(dt1_uniq$patientids[which(dt1_uniq$yy_since1st_cat == 4)])
mat_yys1ct[5,17] <- length(dt1_uniq$patientids[which(dt1_uniq$yy_since1st_cat == 4)])/length(unique(dt1$patientids))*100

mat_yys1ct[5,18] <- sum(dt1_uniq$py[which(dt1_uniq$yy_since1st_cat == 4)])
mat_yys1ct[5,19] <- sum(dt1_uniq$py[which(dt1_uniq$yy_since1st_cat == 4)])/sum(dt1_uniq$py)*100

# Birth cohort
mat_birthcohort <- matrix(nrow = 8, ncol = 15)
rownames(mat_birthcohort) <- c("Birth cohort","<1980","1980-<1985","1985-<1990","1990-<1995","1995-<2000","2000-<2005","???2005")

#<1980
mat_birthcohort[2,1] <- table(dt1$birthcohort == 1 & dt1$all_excl_therap_syndrel == 1)[2]
mat_birthcohort[2,2] <- round((mat_birthcohort[2,1]/mat_overall[1,1])*100,1)
mat_birthcohort[2,3] <- table(dt1$birthcohort == 1 & dt1$Lymphoid == 1)[2] 
mat_birthcohort[2,4] <- table(dt1$birthcohort == 1 & dt1$HL == 1)[2] 
mat_birthcohort[2,5] <- table(dt1$birthcohort == 1 & dt1$NHL == 1)[2] 
mat_birthcohort[2,6] <- table(dt1$birthcohort == 1 & dt1$NHL_Bcell == 1)[2] 
mat_birthcohort[2,7] <- table(dt1$birthcohort == 1 & dt1$NHL_Tcell == 1)[2] 
mat_birthcohort[2,8] <- table(dt1$birthcohort == 1 & dt1$NHL_Precursor_cell == 1)[2] 

mat_birthcohort[2,9] <- table(dt1$birthcohort == 1 & dt1$Myeloid == 1)[2] 
mat_birthcohort[2,10] <- table(dt1$birthcohort == 1 & dt1$AML_prec_ALMP_ALAL == 1)[2] 
mat_birthcohort[2,11] <- table(dt1$birthcohort == 1 & dt1$MPN_MDSMPN_MDS == 1)[2] 
mat_birthcohort[2,12] <- table(dt1$birthcohort == 1 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2] 
mat_birthcohort[2,13] <- table(dt1$birthcohort == 1 & dt1$MPN == 1)[2] 
mat_birthcohort[2,14] <- table(dt1$birthcohort == 1 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2] 
mat_birthcohort[2,15] <- table(dt1$birthcohort == 1 & dt1$UNSP == 1)[2] 

mat_birthcohort[2,16] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 1)])
mat_birthcohort[2,17] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 1)])/length(unique(dt1$patientids))*100

mat_birthcohort[2,18] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 1)])
mat_birthcohort[2,19] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 1)])/sum(dt1_uniq$py)*100

#1980-<1985
mat_birthcohort[3,1] <- table(dt1$birthcohort == 2 & dt1$all_excl_therap_syndrel == 1)[2]
mat_birthcohort[3,2] <- round((mat_birthcohort[3,1]/mat_overall[1,1])*100,1)
mat_birthcohort[3,3] <- table(dt1$birthcohort == 2 & dt1$Lymphoid == 1)[2] 
mat_birthcohort[3,4] <- table(dt1$birthcohort == 2 & dt1$HL == 1)[2] 
mat_birthcohort[3,5] <- table(dt1$birthcohort == 2 & dt1$NHL == 1)[2] 
mat_birthcohort[3,6] <- table(dt1$birthcohort == 2 & dt1$NHL_Bcell == 1)[2] 
mat_birthcohort[3,7] <- table(dt1$birthcohort == 2 & dt1$NHL_Tcell == 1)[2] 
mat_birthcohort[3,8] <- table(dt1$birthcohort == 2 & dt1$NHL_Precursor_cell == 1)[2] 

mat_birthcohort[3,9] <- table(dt1$birthcohort == 2 & dt1$Myeloid == 1)[2] 
mat_birthcohort[3,10] <- table(dt1$birthcohort == 2 & dt1$AML_prec_ALMP_ALAL == 1)[2] 
mat_birthcohort[3,11] <- table(dt1$birthcohort == 2 & dt1$MPN_MDSMPN_MDS == 1)[2] 
mat_birthcohort[3,12] <- table(dt1$birthcohort == 2 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2] 
mat_birthcohort[3,13] <- table(dt1$birthcohort == 2 & dt1$MPN == 1)[2] 
mat_birthcohort[3,14] <- table(dt1$birthcohort == 2 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2] 
mat_birthcohort[3,15] <- table(dt1$birthcohort == 2 & dt1$UNSP == 1)[2] 

mat_birthcohort[3,16] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 2)])
mat_birthcohort[3,17] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 2)])/length(unique(dt1$patientids))*100

mat_birthcohort[3,18] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 2)])
mat_birthcohort[3,19] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 2)])/sum(dt1_uniq$py)*100

#1985-<1990
mat_birthcohort[4,1] <- table(dt1$birthcohort == 3 & dt1$all_excl_therap_syndrel == 1)[2] 
mat_birthcohort[4,2] <- round((mat_birthcohort[4,1]/mat_overall[1,1])*100,1)
mat_birthcohort[4,3] <- table(dt1$birthcohort == 3 & dt1$Lymphoid == 1)[2]  
mat_birthcohort[4,4] <- table(dt1$birthcohort == 3 & dt1$HL == 1)[2]  
mat_birthcohort[4,5] <- table(dt1$birthcohort == 3 & dt1$NHL == 1)[2]  
mat_birthcohort[4,6] <- table(dt1$birthcohort == 3 & dt1$NHL_Bcell == 1)[2]  
mat_birthcohort[4,7] <- table(dt1$birthcohort == 3 & dt1$NHL_Tcell == 1)[2]  
mat_birthcohort[4,8] <- table(dt1$birthcohort == 3 & dt1$NHL_Precursor_cell == 1)[2]  

mat_birthcohort[4,9] <- table(dt1$birthcohort == 3 & dt1$Myeloid == 1)[2]  
mat_birthcohort[4,10] <- table(dt1$birthcohort == 3 & dt1$AML_prec_ALMP_ALAL == 1)[2]  
mat_birthcohort[4,11] <- table(dt1$birthcohort == 3 & dt1$MPN_MDSMPN_MDS == 1)[2]  
mat_birthcohort[4,12] <- table(dt1$birthcohort == 3 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]  
mat_birthcohort[4,13] <- table(dt1$birthcohort == 3 & dt1$MPN == 1)[2]  
mat_birthcohort[4,14] <- table(dt1$birthcohort == 3 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]  
mat_birthcohort[4,15] <- table(dt1$birthcohort == 3 & dt1$UNSP == 1)[2]  

mat_birthcohort[4,16] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 3)])
mat_birthcohort[4,17] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 3)])/length(unique(dt1$patientids))*100

mat_birthcohort[4,18] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 3)])
mat_birthcohort[4,19] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 3)])/sum(dt1_uniq$py)*100

#1990-<1995
mat_birthcohort[5,1] <- table(dt1$birthcohort == 4 & dt1$all_excl_therap_syndrel == 1)[2] 
mat_birthcohort[5,2] <- round((mat_birthcohort[5,1]/mat_overall[1,1])*100,1)
mat_birthcohort[5,3] <- table(dt1$birthcohort == 4 & dt1$Lymphoid == 1)[2]  
mat_birthcohort[5,4] <- table(dt1$birthcohort == 4 & dt1$HL == 1)[2]  
mat_birthcohort[5,5] <- table(dt1$birthcohort == 4 & dt1$NHL == 1)[2]  
mat_birthcohort[5,6] <- table(dt1$birthcohort == 4 & dt1$NHL_Bcell == 1)[2]  
mat_birthcohort[5,7] <- table(dt1$birthcohort == 4 & dt1$NHL_Tcell == 1)[2]  
mat_birthcohort[5,8] <- table(dt1$birthcohort == 4 & dt1$NHL_Precursor_cell == 1)[2]  

mat_birthcohort[5,9] <- table(dt1$birthcohort == 4 & dt1$Myeloid == 1)[2]  
mat_birthcohort[5,10] <- table(dt1$birthcohort == 4 & dt1$AML_prec_ALMP_ALAL == 1)[2]  
mat_birthcohort[5,11] <- table(dt1$birthcohort == 4 & dt1$MPN_MDSMPN_MDS == 1)[2]  
mat_birthcohort[5,12] <- table(dt1$birthcohort == 4 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]  
mat_birthcohort[5,13] <- table(dt1$birthcohort == 4 & dt1$MPN == 1)[2]  
mat_birthcohort[5,14] <- table(dt1$birthcohort == 4 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]  
mat_birthcohort[5,15] <- table(dt1$birthcohort == 4 & dt1$UNSP == 1)[2]  

mat_birthcohort[5,16] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 4)])
mat_birthcohort[5,17] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 4)])/length(unique(dt1$patientids))*100

mat_birthcohort[5,18] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 4)])
mat_birthcohort[5,19] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 4)])/sum(dt1_uniq$py)*100

#1995-<2000
mat_birthcohort[6,1] <- table(dt1$birthcohort == 5 & dt1$all_excl_therap_syndrel == 1)[2] 
mat_birthcohort[6,2] <- round((mat_birthcohort[6,1]/mat_overall[1,1])*100,1)
mat_birthcohort[6,3] <- table(dt1$birthcohort == 5 & dt1$Lymphoid == 1)[2]  
mat_birthcohort[6,4] <- table(dt1$birthcohort == 5 & dt1$HL == 1)[2]  
mat_birthcohort[6,5] <- table(dt1$birthcohort == 5 & dt1$NHL == 1)[2]  
mat_birthcohort[6,6] <- table(dt1$birthcohort == 5 & dt1$NHL_Bcell == 1)[2]  
mat_birthcohort[6,7] <- table(dt1$birthcohort == 5 & dt1$NHL_Tcell == 1)[2]  
mat_birthcohort[6,8] <- table(dt1$birthcohort == 5 & dt1$NHL_Precursor_cell == 1)[2]  

mat_birthcohort[6,9] <- table(dt1$birthcohort == 5 & dt1$Myeloid == 1)[2]  
mat_birthcohort[6,10] <- table(dt1$birthcohort == 5 & dt1$AML_prec_ALMP_ALAL == 1)[2]  
mat_birthcohort[6,11] <- table(dt1$birthcohort == 5 & dt1$MPN_MDSMPN_MDS == 1)[2]  
mat_birthcohort[6,12] <- table(dt1$birthcohort == 5 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]  
mat_birthcohort[6,13] <- table(dt1$birthcohort == 5 & dt1$MPN == 1)[2]  
mat_birthcohort[6,14] <- table(dt1$birthcohort == 5 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]  
mat_birthcohort[6,15] <- table(dt1$birthcohort == 5 & dt1$UNSP == 1)[2]  

mat_birthcohort[6,16] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 5)])
mat_birthcohort[6,17] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 5)])/length(unique(dt1$patientids))*100

mat_birthcohort[6,18] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 5)])
mat_birthcohort[6,19] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 5)])/sum(dt1_uniq$py)*100

#2000-<2005
mat_birthcohort[7,1] <- table(dt1$birthcohort == 6 & dt1$all_excl_therap_syndrel == 1)[2] 
mat_birthcohort[7,2] <- round((mat_birthcohort[7,1]/mat_overall[1,1])*100,1)
mat_birthcohort[7,3] <- table(dt1$birthcohort == 6 & dt1$Lymphoid == 1)[2]  
mat_birthcohort[7,4] <- table(dt1$birthcohort == 6 & dt1$HL == 1)[2]  
mat_birthcohort[7,5] <- table(dt1$birthcohort == 6 & dt1$NHL == 1)[2]  
mat_birthcohort[7,6] <- table(dt1$birthcohort == 6 & dt1$NHL_Bcell == 1)[2]  
mat_birthcohort[7,7] <- table(dt1$birthcohort == 6 & dt1$NHL_Tcell == 1)[2]  
mat_birthcohort[7,8] <- table(dt1$birthcohort == 6 & dt1$NHL_Precursor_cell == 1)[2]  

mat_birthcohort[7,9] <- table(dt1$birthcohort == 6 & dt1$Myeloid == 1)[2]  
mat_birthcohort[7,10] <- table(dt1$birthcohort == 6 & dt1$AML_prec_ALMP_ALAL == 1)[2]  
mat_birthcohort[7,11] <- table(dt1$birthcohort == 6 & dt1$MPN_MDSMPN_MDS == 1)[2]  
mat_birthcohort[7,12] <- table(dt1$birthcohort == 6 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]  
mat_birthcohort[7,13] <- table(dt1$birthcohort == 6 & dt1$MPN == 1)[2]  
mat_birthcohort[7,14] <- table(dt1$birthcohort == 6 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]  
mat_birthcohort[7,15] <- table(dt1$birthcohort == 6 & dt1$UNSP == 1)[2]  

mat_birthcohort[7,16] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 6)])
mat_birthcohort[7,17] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 6)])/length(unique(dt1$patientids))*100

mat_birthcohort[7,18] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 6)])
mat_birthcohort[7,19] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 6)])/sum(dt1_uniq$py)*100

#  ???2005
mat_birthcohort[8,1] <- table(dt1$birthcohort == 7 & dt1$all_excl_therap_syndrel == 1)[2]
mat_birthcohort[8,2] <- round((mat_birthcohort[8,1]/mat_overall[1,1])*100,1)
mat_birthcohort[8,3] <- table(dt1$birthcohort == 7 & dt1$Lymphoid == 1)[2] 
mat_birthcohort[8,4] <- table(dt1$birthcohort == 7 & dt1$HL == 1)[2] 
mat_birthcohort[8,5] <- table(dt1$birthcohort == 7 & dt1$NHL == 1)[2] 
mat_birthcohort[8,6] <- table(dt1$birthcohort == 7 & dt1$NHL_Bcell == 1)[2] 
mat_birthcohort[8,7] <- table(dt1$birthcohort == 7 & dt1$NHL_Tcell == 1)[2] 
mat_birthcohort[8,8] <- table(dt1$birthcohort == 7 & dt1$NHL_Precursor_cell == 1)[2] 

mat_birthcohort[8,9] <- table(dt1$birthcohort == 7 & dt1$Myeloid == 1)[2] 
mat_birthcohort[8,10] <- table(dt1$birthcohort == 7 & dt1$AML_prec_ALMP_ALAL == 1)[2] 
mat_birthcohort[8,11] <- table(dt1$birthcohort == 7 & dt1$MPN_MDSMPN_MDS == 1)[2] 
mat_birthcohort[8,12] <- table(dt1$birthcohort == 7 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2] 
mat_birthcohort[8,13] <- table(dt1$birthcohort == 7 & dt1$MPN == 1)[2] 
mat_birthcohort[8,14] <- table(dt1$birthcohort == 7 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2] 
mat_birthcohort[8,15] <- table(dt1$birthcohort == 7 & dt1$UNSP == 1)[2] 

mat_birthcohort[8,16] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 7)])
mat_birthcohort[8,17] <- length(dt1_uniq$patientids[which(dt1_uniq$birthcohort == 7)])/length(unique(dt1$patientids))*100

mat_birthcohort[8,18] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 7)])
mat_birthcohort[8,19] <- sum(dt1_uniq$py[which(dt1_uniq$birthcohort == 7)])/sum(dt1_uniq$py)*100

# Attained age years
mat_attage <- matrix(nrow = 5, ncol = 15)
rownames(mat_attage) <- c("Attained age. years","2-<20","20-<30","30-<40","???40")


dt1_1 <- dt1[which(dt1$n_pe == 0),]
dt1_1$py <- dt1_1$exit_age - dt1_1$entry_age

#2-<20
mat_attage[2,1] <- table(dt1$agecat == 1 & dt1$all_excl_therap_syndrel == 1)[2]
mat_attage[2,2] <- round((mat_attage[2,1]/mat_overall[1,1])*100,1)
mat_attage[2,3] <- table(dt1$agecat == 1 & dt1$Lymphoid == 1)[2] 
mat_attage[2,4] <- table(dt1$agecat == 1 & dt1$HL == 1)[2] 
mat_attage[2,5] <- table(dt1$agecat == 1 & dt1$NHL == 1)[2] 
mat_attage[2,6] <- table(dt1$agecat == 1 & dt1$NHL_Bcell == 1)[2] 
mat_attage[2,7] <- table(dt1$agecat == 1 & dt1$NHL_Tcell == 1)[2] 
mat_attage[2,8] <- table(dt1$agecat == 1 & dt1$NHL_Precursor_cell == 1)[2] 

mat_attage[2,9] <- table(dt1$agecat == 1 & dt1$Myeloid == 1)[2] 
mat_attage[2,10] <- table(dt1$agecat == 1 & dt1$AML_prec_ALMP_ALAL == 1)[2] 
mat_attage[2,11] <- table(dt1$agecat == 1 & dt1$MPN_MDSMPN_MDS == 1)[2] 
mat_attage[2,12] <- table(dt1$agecat == 1 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2] 
mat_attage[2,13] <- table(dt1$agecat == 1 & dt1$MPN == 1)[2] 
mat_attage[2,14] <- table(dt1$agecat == 1 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2] 
mat_attage[2,15] <- table(dt1$agecat == 1 & dt1$UNSP == 1)[2] 

mat_attage[2,16] <- length(unique(dt1_1$patientids[which(dt1_1$agecat == 1)]))
mat_attage[2,17] <- length(unique(dt1_1$patientids[which(dt1_1$agecat == 1)]))/length(unique(dt1_1$patientids))*100

mat_attage[2,18] <- sum(dt1_1$py[which(dt1_1$agecat == 1)])
mat_attage[2,19] <- sum(dt1_1$py[which(dt1_1$agecat == 1)])/sum(dt1_1$py)*100

#20-<30
mat_attage[3,1] <- table(dt1$agecat == 2 & dt1$all_excl_therap_syndrel == 1)[2] 
mat_attage[3,2] <- round((mat_attage[3,1]/mat_overall[1,1])*100,1)
mat_attage[3,3] <- table(dt1$agecat == 2 & dt1$Lymphoid == 1)[2]  
mat_attage[3,4] <- table(dt1$agecat == 2 & dt1$HL == 1)[2]  
mat_attage[3,5] <- table(dt1$agecat == 2 & dt1$NHL == 1)[2]  
mat_attage[3,6] <- table(dt1$agecat == 2 & dt1$NHL_Bcell == 1)[2]  
mat_attage[3,7] <- table(dt1$agecat == 2 & dt1$NHL_Tcell == 1)[2]  
mat_attage[3,8] <- table(dt1$agecat == 2 & dt1$NHL_Precursor_cell == 1)[2]  

mat_attage[3,9] <- table(dt1$agecat == 2 & dt1$Myeloid == 1)[2]  
mat_attage[3,10] <- table(dt1$agecat == 2 & dt1$AML_prec_ALMP_ALAL == 1)[2]  
mat_attage[3,11] <- table(dt1$agecat == 2 & dt1$MPN_MDSMPN_MDS == 1)[2]  
mat_attage[3,12] <- table(dt1$agecat == 2 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]  
mat_attage[3,13] <- table(dt1$agecat == 2 & dt1$MPN == 1)[2]  
mat_attage[3,14] <- table(dt1$agecat == 2 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]  
mat_attage[3,15] <- table(dt1$agecat == 2 & dt1$UNSP == 1)[2] 

mat_attage[3,16] <- length(unique(dt1_1$patientids[which(dt1_1$agecat == 2)]))
mat_attage[3,17] <- length(unique(dt1_1$patientids[which(dt1_1$agecat == 2)]))/length(unique(dt1_1$patientids))*100

mat_attage[3,18] <- sum(dt1_1$py[which(dt1_1$agecat == 2)])
mat_attage[3,19] <- sum(dt1_1$py[which(dt1_1$agecat == 2)])/sum(dt1_1$py)*100

#30-<40
mat_attage[4,1] <- table(dt1$agecat == 3 & dt1$all_excl_therap_syndrel == 1)[2] 
mat_attage[4,2] <- round((mat_attage[4,1]/mat_overall[1,1])*100,1)
mat_attage[4,3] <- table(dt1$agecat == 3 & dt1$Lymphoid == 1)[2]  
mat_attage[4,4] <- table(dt1$agecat == 3 & dt1$HL == 1)[2]  
mat_attage[4,5] <- table(dt1$agecat == 3 & dt1$NHL == 1)[2]  
mat_attage[4,6] <- table(dt1$agecat == 3 & dt1$NHL_Bcell == 1)[2]  
mat_attage[4,7] <- table(dt1$agecat == 3 & dt1$NHL_Tcell == 1)[2]  
mat_attage[4,8] <- table(dt1$agecat == 3 & dt1$NHL_Precursor_cell == 1)[2]  

mat_attage[4,9] <- table(dt1$agecat == 3 & dt1$Myeloid == 1)[2]  
mat_attage[4,10] <- table(dt1$agecat == 3 & dt1$AML_prec_ALMP_ALAL == 1)[2]  
mat_attage[4,11] <- table(dt1$agecat == 3 & dt1$MPN_MDSMPN_MDS == 1)[2]  
mat_attage[4,12] <- table(dt1$agecat == 3 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]  
mat_attage[4,13] <- table(dt1$agecat == 3 & dt1$MPN == 1)[2]  
mat_attage[4,14] <- table(dt1$agecat == 3 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]  
mat_attage[4,15] <- table(dt1$agecat == 3 & dt1$UNSP == 1)[2] 

mat_attage[4,16] <- length(unique(dt1_1$patientids[which(dt1_1$agecat == 3)]))
mat_attage[4,17] <- length(unique(dt1_1$patientids[which(dt1_1$agecat == 3)]))/length(unique(dt1_1$patientids))*100

mat_attage[4,18] <- sum(dt1_1$py[which(dt1_1$agecat == 3)])
mat_attage[4,19] <- sum(dt1_1$py[which(dt1_1$agecat == 3)])/sum(dt1_1$py)*100

#???40
mat_attage[5,1] <- table(dt1$agecat == 4 & dt1$all_excl_therap_syndrel == 1)[2] 
mat_attage[5,2] <- round((mat_attage[5,1]/mat_overall[1,1])*100,1)
mat_attage[5,3] <- table(dt1$agecat == 4 & dt1$Lymphoid == 1)[2]  
mat_attage[5,4] <- table(dt1$agecat == 4 & dt1$HL == 1)[2]  
mat_attage[5,5] <- table(dt1$agecat == 4 & dt1$NHL == 1)[2]  
mat_attage[5,6] <- table(dt1$agecat == 4 & dt1$NHL_Bcell == 1)[2]  
mat_attage[5,7] <- table(dt1$agecat == 4 & dt1$NHL_Tcell == 1)[2]  
mat_attage[5,8] <- table(dt1$agecat == 4 & dt1$NHL_Precursor_cell == 1)[2]  

mat_attage[5,9] <- table(dt1$agecat == 4 & dt1$Myeloid == 1)[2]  
mat_attage[5,10] <- table(dt1$agecat == 4 & dt1$AML_prec_ALMP_ALAL == 1)[2]  
mat_attage[5,11] <- table(dt1$agecat == 4 & dt1$MPN_MDSMPN_MDS == 1)[2]  
mat_attage[5,12] <- table(dt1$agecat == 4 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]  
mat_attage[5,13] <- table(dt1$agecat == 4 & dt1$MPN == 1)[2]  
mat_attage[5,14] <- table(dt1$agecat == 4 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]  
mat_attage[5,15] <- table(dt1$agecat == 4 & dt1$UNSP == 1)[2] 

mat_attage[5,16] <- length(unique(dt1_1$patientids[which(dt1_1$agecat == 4)]))
mat_attage[5,17] <- length(unique(dt1_1$patientids[which(dt1_1$agecat == 4)]))/length(unique(dt1_1$patientids))*100

mat_attage[5,18] <- sum(dt1_1$py[which(dt1_1$agecat == 4)])
mat_attage[5,19] <- sum(dt1_1$py[which(dt1_1$agecat == 4)])/sum(dt1_1$py)*100



# country
mat_country <- matrix(nrow = 10, ncol = 15)
rownames(mat_country) <- c("Country","Belgium","Denmark","France","Germany","The Netherlands","Norway","Spain","Sweden","UK")

#11 : Belgium 12: Denmark  13: France  14: Germany  15: Netherlands  16: Norway  17: Spain  18: Sweden  19: UK 

#11:Belgium
mat_country[2,1] <- table(dt1$country == 11 & dt1$all_excl_therap_syndrel == 1)[2]
mat_country[2,2] <- round((mat_country[2,1]/mat_overall[1,1])*100,1)
mat_country[2,3] <- table(dt1$country == 11 & dt1$Lymphoid == 1)[2] 
mat_country[2,4] <- table(dt1$country == 11 & dt1$HL == 1)[2]
mat_country[2,5] <- table(dt1$country == 11 & dt1$NHL == 1)[2]
mat_country[2,6] <- table(dt1$country == 11 & dt1$NHL_Bcell == 1)[2]
mat_country[2,7] <- table(dt1$country == 11 & dt1$NHL_Tcell == 1)[2] 
mat_country[2,8] <- table(dt1$country == 11 & dt1$NHL_Precursor_cell == 1)[2]

mat_country[2,9] <- table(dt1$country == 11 & dt1$Myeloid == 1)[2]
mat_country[2,10] <- table(dt1$country == 11 & dt1$AML_prec_ALMP_ALAL == 1)[2]
mat_country[2,11] <- table(dt1$country == 11 & dt1$MPN_MDSMPN_MDS == 1)[2]
mat_country[2,12] <- table(dt1$country == 11 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]
mat_country[2,13] <- table(dt1$country == 11 & dt1$MPN == 1)[2]
mat_country[2,14] <- table(dt1$country == 11 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]
mat_country[2,15] <- table(dt1$country == 11 & dt1$UNSP == 1)[2]

mat_country[2,16] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 11)])
mat_country[2,17] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 11)])/length(unique(dt1$patientids))*100

mat_country[2,18] <- sum(dt1_uniq$py[which(dt1_uniq$country == 11)])
mat_country[2,19] <- sum(dt1_uniq$py[which(dt1_uniq$country == 11)])/sum(dt1_uniq$py)*100

#12: Denmark
mat_country[3,1] <- table(dt1$country == 12 & dt1$all_excl_therap_syndrel == 1)[2]
mat_country[3,2] <- round((mat_country[3,1]/mat_overall[1,1])*100,1)
mat_country[3,3] <- table(dt1$country == 12 & dt1$Lymphoid == 1)[2]
mat_country[3,4] <- table(dt1$country == 12 & dt1$HL == 1)[2]
mat_country[3,5] <- table(dt1$country == 12 & dt1$NHL == 1)[2]
mat_country[3,6] <- table(dt1$country == 12 & dt1$NHL_Bcell == 1)[2]
mat_country[3,7] <- table(dt1$country == 12 & dt1$NHL_Tcell == 1)[2]
mat_country[3,8] <- table(dt1$country == 12 & dt1$NHL_Precursor_cell == 1)[2]

mat_country[3,9] <- table(dt1$country == 12 & dt1$Myeloid == 1)[2]
mat_country[3,10] <- table(dt1$country == 12 & dt1$AML_prec_ALMP_ALAL == 1)[2]
mat_country[3,11] <- table(dt1$country == 12 & dt1$MPN_MDSMPN_MDS == 1)[2]
mat_country[3,12] <- table(dt1$country == 12 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]
mat_country[3,13] <- table(dt1$country == 12 & dt1$MPN == 1)[2]
mat_country[3,14] <- table(dt1$country == 12 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]
mat_country[3,15] <- table(dt1$country == 12 & dt1$UNSP == 1)[2]

mat_country[3,16] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 12)])
mat_country[3,17] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 12)])/length(unique(dt1$patientids))*100

mat_country[3,18] <- sum(dt1_uniq$py[which(dt1_uniq$country == 12)])
mat_country[3,19] <- sum(dt1_uniq$py[which(dt1_uniq$country == 12)])/sum(dt1_uniq$py)*100

#13: France
mat_country[4,1] <- table(dt1$country == 13 & dt1$all_excl_therap_syndrel == 1)[2]
mat_country[4,2] <- round((mat_country[4,1]/mat_overall[1,1])*100,1)
mat_country[4,3] <- table(dt1$country == 13 & dt1$Lymphoid == 1)[2]
mat_country[4,4] <- table(dt1$country == 13 & dt1$HL == 1)[2]
mat_country[4,5] <- table(dt1$country == 13 & dt1$NHL == 1)[2]
mat_country[4,6] <- table(dt1$country == 13 & dt1$NHL_Bcell == 1)[2]
mat_country[4,7] <- table(dt1$country == 13 & dt1$NHL_Tcell == 1)[2]
mat_country[4,8] <- table(dt1$country == 13 & dt1$NHL_Precursor_cell == 1)[2]

mat_country[4,9] <- table(dt1$country == 13 & dt1$Myeloid == 1)[2]
mat_country[4,10] <- table(dt1$country == 13 & dt1$AML_prec_ALMP_ALAL == 1)[2]
mat_country[4,11] <- table(dt1$country == 13 & dt1$MPN_MDSMPN_MDS == 1)[2]
mat_country[4,12] <- table(dt1$country == 13 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]
mat_country[4,13] <- table(dt1$country == 13 & dt1$MPN == 1)[2]
mat_country[4,14] <- table(dt1$country == 13 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]
mat_country[4,15] <- table(dt1$country == 13 & dt1$UNSP == 1)[2]

mat_country[4,16] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 13)])
mat_country[4,17] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 13)])/length(unique(dt1$patientids))*100

mat_country[4,18] <- sum(dt1_uniq$py[which(dt1_uniq$country == 13)])
mat_country[4,19] <- sum(dt1_uniq$py[which(dt1_uniq$country == 13)])/sum(dt1_uniq$py)*100

#14: Germany
mat_country[5,1] <- table(dt1$country == 14 & dt1$all_excl_therap_syndrel == 1)[2]
mat_country[5,2] <- round((mat_country[5,1]/mat_overall[1,1])*100,1)
mat_country[5,3] <- table(dt1$country == 14 & dt1$Lymphoid == 1)[2]
mat_country[5,4] <- table(dt1$country == 14 & dt1$HL == 1)[2]
mat_country[5,5] <- table(dt1$country == 14 & dt1$NHL == 1)[2] 
mat_country[5,6] <- table(dt1$country == 14 & dt1$NHL_Bcell == 1)[2] 
mat_country[5,7] <- table(dt1$country == 14 & dt1$NHL_Tcell == 1)[2]
mat_country[5,8] <- table(dt1$country == 14 & dt1$NHL_Precursor_cell == 1)[2]

mat_country[5,9] <- table(dt1$country == 14 & dt1$Myeloid == 1)[2]
mat_country[5,10] <- table(dt1$country == 14 & dt1$AML_prec_ALMP_ALAL == 1)[2]
mat_country[5,11] <- table(dt1$country == 14 & dt1$MPN_MDSMPN_MDS == 1)[2]
mat_country[5,12] <- table(dt1$country == 14 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]
mat_country[5,13] <- table(dt1$country == 14 & dt1$MPN == 1)[2]
mat_country[5,14] <- table(dt1$country == 14 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]
mat_country[5,15] <- table(dt1$country == 14 & dt1$UNSP == 1)[2]

mat_country[5,16] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 14)])
mat_country[5,17] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 14)])/length(unique(dt1$patientids))*100

mat_country[5,18] <- sum(dt1_uniq$py[which(dt1_uniq$country == 14)])
mat_country[5,19] <- sum(dt1_uniq$py[which(dt1_uniq$country == 14)])/sum(dt1_uniq$py)*100

#15: Netherlands
mat_country[6,1] <- table(dt1$country == 15 & dt1$all_excl_therap_syndrel == 1)[2]
mat_country[6,2] <- round((mat_country[6,1]/mat_overall[1,1])*100,1)
mat_country[6,3] <- table(dt1$country == 15 & dt1$Lymphoid == 1)[2]
mat_country[6,4] <- table(dt1$country == 15 & dt1$HL == 1)[2] 
mat_country[6,5] <- table(dt1$country == 15 & dt1$NHL == 1)[2]
mat_country[6,6] <- table(dt1$country == 15 & dt1$NHL_Bcell == 1)[2]
mat_country[6,7] <- table(dt1$country == 15 & dt1$NHL_Tcell == 1)[2]
mat_country[6,8] <- table(dt1$country == 15 & dt1$NHL_Precursor_cell == 1)[2]

mat_country[6,9] <- table(dt1$country == 15 & dt1$Myeloid == 1)[2]
mat_country[6,10] <- table(dt1$country == 15 & dt1$AML_prec_ALMP_ALAL == 1)[2]
mat_country[6,11] <- table(dt1$country == 15 & dt1$MPN_MDSMPN_MDS == 1)[2]
mat_country[6,12] <- table(dt1$country == 15 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]
mat_country[6,13] <- table(dt1$country == 15 & dt1$MPN == 1)[2]
mat_country[6,14] <- table(dt1$country == 15 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]
mat_country[6,15] <- table(dt1$country == 15 & dt1$UNSP == 1)[2]

mat_country[6,16] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 15)])
mat_country[6,17] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 15)])/length(unique(dt1$patientids))*100

mat_country[6,18] <- sum(dt1_uniq$py[which(dt1_uniq$country == 15)])
mat_country[6,19] <- sum(dt1_uniq$py[which(dt1_uniq$country == 15)])/sum(dt1_uniq$py)*100

#16: Norway
mat_country[7,1] <- table(dt1$country == 16 & dt1$all_excl_therap_syndrel == 1)[2]
mat_country[7,2] <- round((mat_country[7,1]/mat_overall[1,1])*100,1)
mat_country[7,3] <- table(dt1$country == 16 & dt1$Lymphoid == 1)[2]
mat_country[7,4] <- table(dt1$country == 16 & dt1$HL == 1)[2]
mat_country[7,5] <- table(dt1$country == 16 & dt1$NHL == 1)[2]
mat_country[7,6] <- table(dt1$country == 16 & dt1$NHL_Bcell == 1)[2]
mat_country[7,7] <- table(dt1$country == 16 & dt1$NHL_Tcell == 1)[2]
mat_country[7,8] <- table(dt1$country == 16 & dt1$NHL_Precursor_cell == 1)[2] 

mat_country[7,9] <- table(dt1$country == 16 & dt1$Myeloid == 1)[2]
mat_country[7,10] <- table(dt1$country == 16 & dt1$AML_prec_ALMP_ALAL == 1)[2]
mat_country[7,11] <- table(dt1$country == 16 & dt1$MPN_MDSMPN_MDS == 1)[2]
mat_country[7,12] <- table(dt1$country == 16 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]
mat_country[7,13] <- table(dt1$country == 16 & dt1$MPN == 1)[2]
mat_country[7,14] <- table(dt1$country == 16 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]
mat_country[7,15] <- table(dt1$country == 16 & dt1$UNSP == 1)[2]

mat_country[7,16] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 16)])
mat_country[7,17] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 16)])/length(unique(dt1$patientids))*100

mat_country[7,18] <- sum(dt1_uniq$py[which(dt1_uniq$country == 16)])
mat_country[7,19] <- sum(dt1_uniq$py[which(dt1_uniq$country == 16)])/sum(dt1_uniq$py)*100

#17: Spain
mat_country[8,1] <- table(dt1$country == 17 & dt1$all_excl_therap_syndrel == 1)[2]
mat_country[8,2] <- round((mat_country[8,1]/mat_overall[1,1])*100,1)
mat_country[8,3] <- table(dt1$country == 17 & dt1$Lymphoid == 1)[2]
mat_country[8,4] <- table(dt1$country == 17 & dt1$HL == 1)[2]
mat_country[8,5] <- table(dt1$country == 17 & dt1$NHL == 1)[2]
mat_country[8,6] <- table(dt1$country == 17 & dt1$NHL_Bcell == 1)[2]
mat_country[8,7] <- table(dt1$country == 17 & dt1$NHL_Tcell == 1)[2]
mat_country[8,8] <- table(dt1$country == 17 & dt1$NHL_Precursor_cell == 1)[2]

mat_country[8,9] <- table(dt1$country == 17 & dt1$Myeloid == 1)[2]
mat_country[8,10] <- table(dt1$country == 17 & dt1$AML_prec_ALMP_ALAL == 1)[2]
mat_country[8,11] <- table(dt1$country == 17 & dt1$MPN_MDSMPN_MDS == 1)[2]
mat_country[8,12] <- table(dt1$country == 17 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]
mat_country[8,13] <- table(dt1$country == 17 & dt1$MPN == 1)[2]
mat_country[8,14] <- table(dt1$country == 17 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]
mat_country[8,15] <- table(dt1$country == 17 & dt1$UNSP == 1)[2]

mat_country[8,16] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 17)])
mat_country[8,17] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 17)])/length(unique(dt1$patientids))*100

mat_country[8,18] <- sum(dt1_uniq$py[which(dt1_uniq$country == 17)])
mat_country[8,19] <- sum(dt1_uniq$py[which(dt1_uniq$country == 17)])/sum(dt1_uniq$py)*100

#18: Sweden
mat_country[9,1] <- table(dt1$country == 18 & dt1$all_excl_therap_syndrel == 1)[2]
mat_country[9,2] <- round((mat_country[9,1]/mat_overall[1,1])*100,1)
mat_country[9,3] <- table(dt1$country == 18 & dt1$Lymphoid == 1)[2]
mat_country[9,4] <- table(dt1$country == 18 & dt1$HL == 1)[2] 
mat_country[9,5] <- table(dt1$country == 18 & dt1$NHL == 1)[2]
mat_country[9,6] <- table(dt1$country == 18 & dt1$NHL_Bcell == 1)[2]
mat_country[9,7] <- table(dt1$country == 18 & dt1$NHL_Tcell == 1)[2]
mat_country[9,8] <- table(dt1$country == 18 & dt1$NHL_Precursor_cell == 1)[2]

mat_country[9,9] <- table(dt1$country == 18 & dt1$Myeloid == 1)[2]
mat_country[9,10] <- table(dt1$country == 18 & dt1$AML_prec_ALMP_ALAL == 1)[2]
mat_country[9,11] <- table(dt1$country == 18 & dt1$MPN_MDSMPN_MDS == 1)[2]
mat_country[9,12] <- table(dt1$country == 18 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]
mat_country[9,13] <- table(dt1$country == 18 & dt1$MPN == 1)[2]
mat_country[9,14] <- table(dt1$country == 18 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]
mat_country[9,15] <- table(dt1$country == 18 & dt1$UNSP == 1)[2]

mat_country[9,16] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 18)])
mat_country[9,17] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 18)])/length(unique(dt1$patientids))*100

mat_country[9,18] <- sum(dt1_uniq$py[which(dt1_uniq$country == 18)])
mat_country[9,19] <- sum(dt1_uniq$py[which(dt1_uniq$country == 18)])/sum(dt1_uniq$py)*100

#19: UK 
mat_country[10,1] <- table(dt1$country == 19 & dt1$all_excl_therap_syndrel == 1)[2]
mat_country[10,2] <- round((mat_country[10,1]/mat_overall[1,1])*100,1)
mat_country[10,3] <- table(dt1$country == 19 & dt1$Lymphoid == 1)[2]
mat_country[10,4] <- table(dt1$country == 19 & dt1$HL == 1)[2] 
mat_country[10,5] <- table(dt1$country == 19 & dt1$NHL == 1)[2] 
mat_country[10,6] <- table(dt1$country == 19 & dt1$NHL_Bcell == 1)[2]
mat_country[10,7] <- table(dt1$country == 19 & dt1$NHL_Tcell == 1)[2]
mat_country[10,8] <- table(dt1$country == 19 & dt1$NHL_Precursor_cell == 1)[2]

mat_country[10,9] <- table(dt1$country == 19 & dt1$Myeloid == 1)[2]
mat_country[10,10] <- table(dt1$country == 19 & dt1$AML_prec_ALMP_ALAL == 1)[2]
mat_country[10,11] <- table(dt1$country == 19 & dt1$MPN_MDSMPN_MDS == 1)[2]
mat_country[10,12] <- table(dt1$country == 19 & dt1$AML_AL_ALMP_AL_excl_gen == 1)[2]
mat_country[10,13] <- table(dt1$country == 19 & dt1$MPN == 1)[2]
mat_country[10,14] <- table(dt1$country == 19 & dt1$HISTIOCYTIC_DENDRITIC == 1)[2]
mat_country[10,15] <- table(dt1$country == 19 & dt1$UNSP == 1)[2]

mat_country[10,16] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 19)])
mat_country[10,17] <- length(dt1_uniq$patientids[which(dt1_uniq$country == 19)])/length(unique(dt1$patientids))*100

mat_country[10,18] <- sum(dt1_uniq$py[which(dt1_uniq$country == 19)])
mat_country[10,19] <- sum(dt1_uniq$py[which(dt1_uniq$country == 19)])/sum(dt1_uniq$py)*100


#Mean ABM dose (range), mGy
names(dt1)

mat_ABM <- matrix(nrow = 3, ncol = 17)

mat_ABM[1,1] <- round(mean(dt1$dose_cum[which(dt1$all_excl_therap_syndrel == 1)]))
mat_ABM[1,3] <- round(mean(dt1$dose_cum[which(dt1$Lymphoid == 1)]))
mat_ABM[1,4] <- round(mean(dt1$dose_cum[which(dt1$HL == 1)]))
mat_ABM[1,5] <- round(mean(dt1$dose_cum[which(dt1$NHL == 1)]))
mat_ABM[1,6] <- round(mean(dt1$dose_cum[which(dt1$NHL_Bcell == 1)]))
mat_ABM[1,7] <- round(mean(dt1$dose_cum[which(dt1$NHL_Tcell == 1)]))
mat_ABM[1,8] <- round(mean(dt1$dose_cum[which(dt1$NHL_Precursor_cell == 1)]))
mat_ABM[1,9] <- round(mean(dt1$dose_cum[which(dt1$Myeloid == 1)]))
mat_ABM[1,10] <- round(mean(dt1$dose_cum[which(dt1$AML_prec_ALMP_ALAL == 1)]))
mat_ABM[1,11] <- round(mean(dt1$dose_cum[which(dt1$MPN_MDSMPN_MDS == 1)]))
mat_ABM[1,12] <- round(mean(dt1$dose_cum[which(dt1$AML_AL_ALMP_AL_excl_gen == 1)]))
mat_ABM[1,13] <- round(mean(dt1$dose_cum[which(dt1$MPN == 1)]))
mat_ABM[1,14] <- round(mean(dt1$dose_cum[which(dt1$HISTIOCYTIC_DENDRITIC == 1)]))
mat_ABM[1,15] <- round(mean(dt1$dose_cum[which(dt1$UNSP == 1)]))
mat_ABM[1,16] <- round(mean(dt1_1$dose_cum),1)

mat_ABM[2,1] <- min(dt1$dose_cum[which(dt1$all_excl_therap_syndrel == 1)])
mat_ABM[2,3] <- min(dt1$dose_cum[which(dt1$Lymphoid == 1)])
mat_ABM[2,4] <- min(dt1$dose_cum[which(dt1$HL == 1)])
mat_ABM[2,5] <- min(dt1$dose_cum[which(dt1$NHL == 1)])
mat_ABM[2,6] <- min(dt1$dose_cum[which(dt1$NHL_Bcell == 1)])
mat_ABM[2,7] <- min(dt1$dose_cum[which(dt1$NHL_Tcell == 1)])
mat_ABM[2,8] <- min(dt1$dose_cum[which(dt1$NHL_Precursor_cell == 1)])
mat_ABM[2,9] <- min(dt1$dose_cum[which(dt1$Myeloid == 1)])
mat_ABM[2,10] <- min(dt1$dose_cum[which(dt1$AML_prec_ALMP_ALAL == 1)])
mat_ABM[2,11] <- min(dt1$dose_cum[which(dt1$MPN_MDSMPN_MDS == 1)])
mat_ABM[2,12] <- min(dt1$dose_cum[which(dt1$AML_AL_ALMP_AL_excl_gen == 1)])
mat_ABM[2,13] <- min(dt1$dose_cum[which(dt1$MPN == 1)])
mat_ABM[2,14] <- min(dt1$dose_cum[which(dt1$HISTIOCYTIC_DENDRITIC == 1)])
mat_ABM[2,15] <- min(dt1$dose_cum[which(dt1$UNSP == 1)])
mat_ABM[2,16] <- min(dt1_1$dose_cum)

mat_ABM[3,1] <- max(dt1$dose_cum[which(dt1$all_excl_therap_syndrel == 1)])
mat_ABM[3,3] <- max(dt1$dose_cum[which(dt1$Lymphoid == 1)])
mat_ABM[3,4] <- max(dt1$dose_cum[which(dt1$HL == 1)])
mat_ABM[3,5] <- max(dt1$dose_cum[which(dt1$NHL == 1)])
mat_ABM[3,6] <- max(dt1$dose_cum[which(dt1$NHL_Bcell == 1)])
mat_ABM[3,7] <- max(dt1$dose_cum[which(dt1$NHL_Tcell == 1)])
mat_ABM[3,8] <- max(dt1$dose_cum[which(dt1$NHL_Precursor_cell == 1)])
mat_ABM[3,9] <- max(dt1$dose_cum[which(dt1$Myeloid == 1)])
mat_ABM[3,10] <- max(dt1$dose_cum[which(dt1$AML_prec_ALMP_ALAL == 1)])
mat_ABM[3,11] <- max(dt1$dose_cum[which(dt1$MPN_MDSMPN_MDS == 1)])
mat_ABM[3,12] <- max(dt1$dose_cum[which(dt1$AML_AL_ALMP_AL_excl_gen == 1)])
mat_ABM[3,13] <- max(dt1$dose_cum[which(dt1$MPN == 1)])
mat_ABM[3,14] <- max(dt1$dose_cum[which(dt1$HISTIOCYTIC_DENDRITIC == 1)])
mat_ABM[3,15] <- max(dt1$dose_cum[which(dt1$UNSP == 1)])
mat_ABM[3,16] <- max(dt1_1$dose_cum)

mat <- rbind(mat_overall,mat_sex, mat_agecat, mat_yys1ct, mat_birthcohort, mat_attage, mat_country, mat_ABM)









