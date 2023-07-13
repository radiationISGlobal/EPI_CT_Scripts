### SUPPLEMENTARY MATERIAL_EPI-CT
### Extended Data Table 1: Number of CT examinations and cumulative dose to the active bone marrow per subject, for all participants and by country




library(dplyr)
library(tidyr)


dt1 <- readRDS("transformed_cohorts/dt1_new_outcomes.rds")


##### examination by row
dt1_1 <- dt1[which(dt1$n_pe != -1 & dt1$n_pe != 0),]
n_ct <- dt1_1 %>% group_by(patientids) %>% mutate(n_ct = n())


#num of examinations by patient, remove duplicates keeping the cumulative dose
n_ct_unique <- n_ct %>% group_by(patientids) %>% filter(n_pe == max(n_pe))

countries <- c(NA,11:19)

## Mean number of CTs per patient

mat_1 <- matrix(ncol = 1, nrow = 11)

mat_1[1,1] <- mean(n_ct_unique$n_ct)
for(i in 2:10){
  mat_1[i,1] <- mean(n_ct_unique$n_ct[which(n_ct_unique$country == countries[i])])
}

## Total number of CTs exams

mat_2 <- matrix(ncol = 1, nrow = 11)

mat_2[1,1] <- sum(n_ct_unique$n_ct)  
for (i in 2:10) {
  mat_2[i,1] <- sum(n_ct_unique$n_ct[which(n_ct_unique$country == countries[i])])
}  
  

# Estimated cumulative ABM dose per patient
# mean 
mat_3 <- matrix(ncol = 1, nrow = 11)

mat_3[1,1] <- mean(n_ct_unique$dose_cum)
for (i in 2:10) {
  mat_3[i,1] <- mean(n_ct_unique$dose_cum[which(n_ct_unique$country == countries[i])])
}

#median
mat_4 <- matrix(ncol = 1, nrow = 11)
mat_4[1,1] <- median(n_ct_unique$dose_cum)
for (i in 2:10) {
  mat_4[i,1] <- median(n_ct_unique$dose_cum[which(n_ct_unique$country == countries[i])])
}

#Min
mat_5 <- matrix(ncol = 1, nrow = 11)
mat_5[1,1] <- min(n_ct_unique$dose_cum)
for (i in 2:10) {
  mat_5[i,1] <- min(n_ct_unique$dose_cum[which(n_ct_unique$country == countries[i])])
}

#Max
mat_6 <- matrix(ncol = 1, nrow = 11)
mat_6[1,1] <- max(n_ct_unique$dose_cum)
for (i in 2:10) {
  mat_6[i,1] <- max(n_ct_unique$dose_cum[which(n_ct_unique$country == countries[i])])
}

# Number of subjects

mat_7 <- matrix(ncol = 1, nrow = 11)
mat_7[1,1] <- length(n_ct_unique$patientids)
for (i in 2:10) {
  mat_7[i,1] <- length(n_ct_unique$patientids[which(n_ct_unique$country == countries[i])])
}


mat <- cbind(mat_1,mat_2,mat_3,mat_4,mat_5,mat_6,mat_7)


